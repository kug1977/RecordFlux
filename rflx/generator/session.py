from typing import Callable, Iterable, List, Mapping, Sequence, Tuple

from rflx import declaration as decl, expression as expr, model, statement as stmt, typing_ as rty
from rflx.ada import (
    FALSE,
    ID,
    TRUE,
    Add,
    And,
    Assignment,
    Call,
    CallStatement,
    CaseStatement,
    Declaration,
    Declare,
    EnumerationType,
    Expr,
    First,
    FormalPackageDeclaration,
    GreaterEqual,
    IfStatement,
    Less,
    NamedAggregate,
    New,
    Not,
    NotEqual,
    Number,
    ObjectDeclaration,
    Or,
    OutParameter,
    PackageUnit,
    Postcondition,
    Pragma,
    PragmaStatement,
    Precondition,
    ProcedureSpecification,
    QualifiedExpr,
    ReturnStatement,
    Size,
    Statement,
    String,
    SubprogramBody,
    SubprogramDeclaration,
    UnitPart,
    UsePackageClause,
    UseTypeClause,
    ValueRange,
    Variable,
    While,
    WithClause,
)
from rflx.common import flat_name
from rflx.const import BUILTINS_PACKAGE, INTERNAL_PACKAGE

from . import common, const


class SessionFlags:
    types: bool = False
    types_bit_length: bool = False


class StateFlags:
    exception: bool = False


class SessionGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.__prefix = prefix

    def create_generic_session(self, unit: PackageUnit, session: model.Session) -> None:
        for type_ in session.types.values():
            if type_.package in [BUILTINS_PACKAGE, INTERNAL_PACKAGE]:
                continue

            if isinstance(type_, model.Scalar) and type_.package != session.package:
                unit.declaration_context.extend(
                    [
                        WithClause(self.__prefix * ID(type_.package)),
                        UsePackageClause(self.__prefix * ID(type_.package)),
                    ]
                )

            elif isinstance(type_, model.Array):
                element_type_identifier = ID(flat_name(str(type_.element_type.identifier)))
                if isinstance(type_.element_type, model.Message):
                    name = const.MESSAGE_SEQUENCE_PACKAGE
                elif isinstance(type_.element_type, model.Scalar):
                    name = const.SCALAR_SEQUENCE_PACKAGE
                else:
                    assert False, 'unexpected element type "{type(type_.element_type)}"'
                unit.declaration_context.append(WithClause(self.__prefix * name))
                assert unit.declaration.formal_parameters
                if isinstance(type_.element_type, model.Message):
                    unit.declaration.formal_parameters.append(
                        FormalPackageDeclaration(
                            flat_name(str(type_.identifier)),
                            self.__prefix * name,
                            [
                                "Types",
                                element_type_identifier * "Context",
                                element_type_identifier * "Initialize",
                                element_type_identifier * "Take_Buffer",
                                element_type_identifier * "Has_Buffer",
                                element_type_identifier * "Message_Last",
                                element_type_identifier * "Initialized",
                                element_type_identifier * "Structural_Valid_Message",
                            ],
                        )
                    )
                elif isinstance(type_.element_type, model.Scalar):
                    unit.declaration.formal_parameters.append(
                        FormalPackageDeclaration(
                            flat_name(str(type_.identifier)),
                            self.__prefix * name,
                            [
                                "Types",
                                self.__prefix * ID(type_.element_type.identifier),
                                self.__prefix
                                * ID(type_.element_type.package)
                                * (
                                    common.base_type_name(type_.element_type)
                                    if not isinstance(type_.element_type, model.ModularInteger)
                                    else element_type_identifier.name
                                ),
                                self.__prefix * ID(type_.element_type.package) * "Valid",
                                self.__prefix * ID(type_.element_type.package) * "To_Actual",
                                self.__prefix * ID(type_.element_type.package) * "To_Base",
                            ],
                        )
                    )
                else:
                    assert False, 'unexpected element type "{type_.type(element_type)}"'

            elif isinstance(type_, model.Message):
                unit.declaration_context.extend(
                    [
                        WithClause(common.generic_name(self.__prefix * ID(type_.identifier))),
                        UsePackageClause(self.__prefix * ID(type_.package)),
                    ]
                )
                assert unit.declaration.formal_parameters
                unit.declaration.formal_parameters.append(
                    FormalPackageDeclaration(
                        flat_name(str(type_.identifier)),
                        common.generic_name(self.__prefix * ID(type_.identifier)),
                        ["Types", "others => <>"],
                    )
                )

        session_flags = SessionFlags()

        declarations, initialization, finalization, invariant = eval_declarations(
            session.declarations.values(),
            session.types,
            session.package,
            session_flags,
            session_global=True,
        )

        unit += self.create_declarations(declarations)
        unit += self.create_states(session, session_flags)
        unit += self.create_run_procedure(session, initialization, finalization, invariant)

        if session_flags.types:
            used_types = [const.TYPES_INDEX]
            if session_flags.types_bit_length:
                used_types.append(const.TYPES_BIT_LENGTH)
            unit.body.declarations = [
                UseTypeClause(
                    *used_types
                    # const.TYPES_BYTES_PTR,
                    # const.TYPES_U64,
                ),
                *unit.body.declarations,
            ]
        if not session_flags.types:
            unit.declaration.declarations = [
                Pragma("Unreferenced", [Variable("Types")]),
                *unit.declaration.declarations,
            ]

    @staticmethod
    def create_declarations(declarations: Sequence[Declaration]) -> UnitPart:
        return UnitPart(
            [
                ObjectDeclaration(["Executed"], "Boolean", FALSE),
            ],
            declarations,
        )

    @staticmethod
    def create_states(session: model.Session, session_flags: SessionFlags) -> UnitPart:
        invariants = [
            Call(
                f"{flat_name(str(d.type_identifier))}.Has_Buffer",
                [Variable(f"{d.identifier}_Ctx")],
            )
            for d in session.declarations.values()
            if isinstance(d, decl.VariableDeclaration)
            and isinstance(
                session.types[model.qualified_type_identifier(d.type_identifier, session.package)],
                (model.Message, model.Array),
            )
        ]

        unit_body: List[Declaration] = [
            EnumerationType(
                "Session_State", {ID(f"S_{s.identifier}"): None for s in session.states}
            )
        ]

        for state in session.states:
            if not state.is_null:
                declarations, initialization, finalization, _ = eval_declarations(
                    state.declarations.values(), session.types, session.package, session_flags
                )
                state_flags = StateFlags()
                actions = [
                    s
                    for a in state.actions
                    for s in state_action(
                        a, state, session, session_flags, state_flags, finalization
                    )
                ]
                if state_flags.exception:
                    declarations.append(ObjectDeclaration(["RFLX_Exception"], "Boolean", FALSE))

                unit_body += [
                    SubprogramBody(
                        ProcedureSpecification(
                            ID(state.identifier), [OutParameter(["State"], "Session_State")]
                        ),
                        declarations,
                        [
                            *initialization,
                            *actions,
                            *(
                                [
                                    IfStatement(
                                        [
                                            (
                                                t.condition.substituted(
                                                    session_substitution(session)
                                                ).ada_expr(),
                                                [Assignment("State", Variable(f"S_{t.target}"))],
                                            )
                                            for t in state.transitions[:-1]
                                        ],
                                        [
                                            Assignment(
                                                "State",
                                                Variable(f"S_{state.transitions[-1].target}"),
                                            )
                                        ],
                                    )
                                ]
                                if state.transitions
                                else []
                            ),
                            *finalization,
                        ],
                        aspects=[
                            Precondition(And(*invariants)),
                            Postcondition(And(*invariants)),
                        ]
                        if invariants
                        else [],
                    )
                ]

        return UnitPart(body=unit_body)

    @staticmethod
    def create_run_procedure(
        session: model.Session,
        initialization: Sequence[Statement],
        finalization: Sequence[Statement],
        invariant: Expr,
    ) -> UnitPart:
        return UnitPart(
            [
                Pragma("Warnings", [Variable("Off"), String('subprogram ""Run"" has no effect')]),
                SubprogramDeclaration(
                    ProcedureSpecification("Run"),
                ),
                Pragma("Warnings", [Variable("On"), String('subprogram ""Run"" has no effect')]),
            ],
            [
                SubprogramBody(
                    ProcedureSpecification("Run"),
                    [
                        ObjectDeclaration(
                            ["State"], "Session_State", Variable(f"S_{session.initial}")
                        ),
                    ],
                    [
                        *initialization,
                        While(
                            NotEqual(Variable("State"), Variable(f"S_{session.final}")),
                            [
                                *(
                                    [PragmaStatement("Loop_Invariant", [invariant])]
                                    if invariant != And()
                                    else []
                                ),
                                CaseStatement(
                                    Variable("State"),
                                    [
                                        (
                                            Variable(f"S_{s.identifier}"),
                                            [
                                                CallStatement(ID(s.identifier), [Variable("State")])
                                                if s.identifier != session.final
                                                else Assignment("Executed", TRUE)
                                            ],
                                        )
                                        for s in session.states
                                    ],
                                ),
                            ],
                        ),
                        *finalization,
                    ],
                )
            ],
        )


def eval_declarations(
    declarations: Iterable[decl.Declaration],
    session_types: Mapping[expr.ID, model.Type],
    session_package: expr.ID,
    session_flags: SessionFlags,
    session_global: bool = False,
) -> Tuple[List[Declaration], List[Statement], List[Statement], Expr]:

    result_declarations: List[Declaration] = []
    initialization: List[Statement] = []
    finalization: List[Statement] = []
    invariant: List[Expr] = []

    for declaration in declarations:
        if isinstance(declaration, decl.VariableDeclaration):
            if isinstance(
                session_types[
                    model.qualified_type_identifier(declaration.type_identifier, session_package)
                ],
                (model.Message, model.Array),
            ):
                assert declaration.expression is None

                session_flags.types = True

                result_declarations.extend(
                    [
                        ObjectDeclaration(
                            [ID(f"{declaration.identifier}_Ctx")],
                            ID(flat_name(str(declaration.type_identifier))) * "Context",
                        ),
                        ObjectDeclaration(
                            [ID(f"{declaration.identifier}_Buffer")], const.TYPES_BYTES_PTR
                        ),
                    ]
                )
                reset_message_contexts = IfStatement(
                    [
                        (
                            Call(
                                f"{flat_name(str(declaration.type_identifier))}.Has_Buffer",
                                [Variable(f"{declaration.identifier}_Ctx")],
                            ),
                            [
                                PragmaStatement(
                                    "Warnings",
                                    [
                                        Variable("Off"),
                                        String(
                                            f'unused assignment to ""{declaration.identifier}_Ctx""'
                                        ),
                                    ],
                                ),
                                CallStatement(
                                    ID(flat_name(str(declaration.type_identifier))) * "Take_Buffer",
                                    [
                                        Variable(f"{declaration.identifier}_Ctx"),
                                        Variable(f"{declaration.identifier}_Buffer"),
                                    ],
                                ),
                                PragmaStatement(
                                    "Warnings",
                                    [
                                        Variable("On"),
                                        String(
                                            f'unused assignment to ""{declaration.identifier}_Ctx""'
                                        ),
                                    ],
                                ),
                                CallStatement(
                                    "Types.Free",
                                    [Variable(f"{declaration.identifier}_Buffer")],
                                ),
                            ],
                        )
                    ]
                )
                initialization.extend(
                    [
                        *([reset_message_contexts] if session_global else []),
                        Assignment(
                            ID(f"{declaration.identifier}_Buffer"),
                            New(
                                QualifiedExpr(
                                    const.TYPES_BYTES,
                                    NamedAggregate(
                                        (
                                            ValueRange(
                                                First(const.TYPES_INDEX),
                                                Add(First(const.TYPES_INDEX), Number(4095)),
                                            ),
                                            First(const.TYPES_BYTE),
                                        )
                                    ),
                                )
                            ),
                        ),
                        CallStatement(
                            ID(flat_name(str(declaration.type_identifier))) * "Initialize",
                            [
                                Variable(f"{declaration.identifier}_Ctx"),
                                Variable(f"{declaration.identifier}_Buffer"),
                            ],
                        ),
                    ]
                )
                finalization.extend(
                    [
                        PragmaStatement(
                            "Warnings",
                            [
                                Variable("Off"),
                                String(f'unused assignment to ""{declaration.identifier}_Ctx""'),
                            ],
                        ),
                        CallStatement(
                            ID(flat_name(str(declaration.type_identifier))) * "Take_Buffer",
                            [
                                Variable(f"{declaration.identifier}_Ctx"),
                                Variable(f"{declaration.identifier}_Buffer"),
                            ],
                        ),
                        PragmaStatement(
                            "Warnings",
                            [
                                Variable("On"),
                                String(f'unused assignment to ""{declaration.identifier}_Ctx""'),
                            ],
                        ),
                        CallStatement("Types.Free", [Variable(f"{declaration.identifier}_Buffer")]),
                    ]
                )
                invariant.append(
                    Call(
                        f"{flat_name(str(declaration.type_identifier))}.Has_Buffer",
                        [Variable(f"{declaration.identifier}_Ctx")],
                    ),
                )

            else:
                result_declarations.append(
                    ObjectDeclaration(
                        [ID(declaration.identifier)],
                        ID(declaration.type_identifier),
                        declaration.expression.ada_expr() if declaration.expression else None,
                    )
                )

        else:
            assert False, f'unexpected declaration type "{type(declaration)}"'

    return (result_declarations, initialization, finalization, And(*invariant))


def state_action(
    action: stmt.Statement,
    state: model.State,
    session: model.Session,
    session_flags: SessionFlags,
    state_flags: StateFlags,
    finalization: Sequence[Statement],
) -> Sequence[Statement]:
    if isinstance(action, stmt.Assignment):
        if isinstance(action.expression, expr.Comprehension):
            assert isinstance(action.type_, rty.Array)
            assert isinstance(action.type_.element, (rty.Integer, rty.Enumeration))
            assert isinstance(action.expression.array.type_, rty.Array)

            session_flags.types_bit_length = True
            state_flags.exception = True

            target_type = ID(flat_name(action.type_.name))
            target_context = ID(f"{action.identifier}_Ctx")
            target_element_type = ID(expr.ID(action.type_.element.name))
            sequence_type = ID(flat_name(action.expression.array.type_.name))
            assert isinstance(action.expression.array, expr.Variable)  # FIXME
            sequence_context = ID(expr.ID(f"{action.expression.array}_Ctx"))
            iterator_context = ID(f"{action.expression.iterator}_Ctx")

            if isinstance(action.expression.array.type_.element, rty.Message):
                iterator_type = ID(flat_name(str(action.expression.array.type_.element.name)))
                return [
                    While(
                        Call(f"{sequence_type}.Has_Element", [Variable(sequence_context)]),
                        [
                            PragmaStatement(
                                "Loop_Invariant",
                                [Call(f"{sequence_type}.Has_Buffer", [Variable(sequence_context)])],
                            ),
                            PragmaStatement(
                                "Loop_Invariant",
                                [Call(f"{target_type}.Has_Buffer", [Variable(target_context)])],
                            ),
                            PragmaStatement(
                                "Loop_Invariant",
                                [Call(f"{target_type}.Valid", [Variable(target_context)])],
                            ),
                            Declare(
                                [
                                    ObjectDeclaration(
                                        [iterator_context], iterator_type * "Context"
                                    ),
                                ],
                                [
                                    CallStatement(
                                        f"{target_type}.Reset", [Variable(target_context)]
                                    ),
                                    CallStatement(
                                        f"{sequence_type}.Switch",
                                        [Variable(sequence_context), Variable(iterator_context)],
                                    ),
                                    CallStatement(
                                        f"{iterator_type}.Verify_Message",
                                        [Variable(iterator_context)],
                                    ),
                                    if_valid_fields(
                                        action.expression.condition,
                                        [
                                            IfStatement(
                                                [
                                                    (
                                                        action.expression.condition.substituted(
                                                            session_substitution(session)
                                                        ).ada_expr(),
                                                        [
                                                            if_sufficient_space(
                                                                expr.Size(target_element_type),
                                                                target_type,
                                                                target_context,
                                                                [
                                                                    CallStatement(
                                                                        f"{target_type}.Append_Element",
                                                                        [
                                                                            Variable(
                                                                                target_context
                                                                            ),
                                                                            action.expression.selector.substituted(
                                                                                session_substitution(
                                                                                    session
                                                                                )
                                                                            ).ada_expr(),
                                                                        ],
                                                                    )
                                                                ],
                                                            ),
                                                        ],
                                                    )
                                                ]
                                            )
                                        ],
                                        session,
                                    ),
                                    PragmaStatement(
                                        "Warnings",
                                        [
                                            Variable("Off"),
                                            String(f'unused assignment to ""{iterator_context}""'),
                                        ],
                                    ),
                                    CallStatement(
                                        f"{sequence_type}.Update",
                                        [Variable(sequence_context), Variable(iterator_context)],
                                    ),
                                    PragmaStatement(
                                        "Warnings",
                                        [
                                            Variable("On"),
                                            String(f'unused assignment to ""{iterator_context}""'),
                                        ],
                                    ),
                                ],
                            ),
                            IfStatement(
                                [
                                    (
                                        Variable("RFLX_Exception"),
                                        [
                                            Assignment(
                                                "State",
                                                Variable(f"S_{state.transitions[-1].target}"),
                                            ),
                                            *finalization,
                                            ReturnStatement(),
                                        ],
                                    )
                                ]
                            ),
                        ],
                    )
                ]

            assert False, f'unexpected element type "{type(action.expression.array.type_.element)}"'

        return [
            Assignment(
                ID(action.identifier),
                action.expression.substituted(session_substitution(session)).ada_expr(),
            )
        ]

    if isinstance(action, stmt.Append):
        assert state.exception_transition

        session_flags.types_bit_length = True

        def check(
            sequence_type: ID, required_space: Expr, exception_transition_target: ID
        ) -> Statement:
            return IfStatement(
                [
                    (
                        Or(
                            Not(
                                Call(
                                    f"{sequence_type}.Has_Element",
                                    [Variable(sequence_context)],
                                )
                            ),
                            Less(
                                Call(
                                    f"{sequence_type}.Available_Space",
                                    [Variable(sequence_context)],
                                ),
                                required_space,
                            ),
                        ),
                        [
                            Assignment("State", Variable(f"S_{exception_transition_target}")),
                            *finalization,
                            ReturnStatement(),
                        ],
                    )
                ]
            )

        if isinstance(action.type_, rty.Array) and isinstance(
            action.type_.element, (rty.Integer, rty.Enumeration)
        ):
            assert isinstance(action.parameter, expr.Variable)

            sequence_type = ID(flat_name(action.type_.name))
            sequence_context = ID(f"{action.identifier}_Ctx")
            element_type = ID(expr.ID(action.type_.element.name))

            return [
                check(sequence_type, Size(element_type), ID(state.exception_transition.target)),
                CallStatement(
                    f"{sequence_type}.Append_Element",
                    [Variable(sequence_context), action.parameter.ada_expr()],
                ),
            ]

        if isinstance(action.type_, rty.Array) and isinstance(action.type_.element, rty.Message):
            sequence_type = ID(flat_name(action.type_.name))
            sequence_context = ID(f"{action.identifier}_Ctx")
            element_type = ID(flat_name(action.type_.element.name))
            element_context = ID(f"{action.identifier}_Element_Ctx")

            if isinstance(action.parameters[0], expr.MessageAggregate):
                parameter = action.parameters[0]

                message = session.types[parameter.identifier]
                assert isinstance(message, model.Message)
                message_size = message.size(
                    {model.Field(f): v for f, v in parameter.field_values.items()}
                ).ada_expr()
            else:
                message_size = Number(0)  # FIXME

            return [
                check(sequence_type, message_size, ID(state.exception_transition.target)),
                Declare(
                    [ObjectDeclaration([element_context], element_type * "Context")],
                    [
                        CallStatement(
                            f"{sequence_type}.Switch",
                            [Variable(sequence_context), Variable(element_context)],
                        ),
                        *(
                            [
                                CallStatement(
                                    f"{element_type}.Set_{f}",
                                    [
                                        Variable(element_context),
                                        v.substituted(session_substitution(session)).ada_expr(),
                                    ],
                                )
                                for f, v in action.parameters[0].field_values.items()
                            ]
                            if isinstance(action.parameters[0], expr.MessageAggregate)
                            else []
                        ),
                        PragmaStatement(
                            "Warnings",
                            [
                                Variable("Off"),
                                String(f'unused assignment to ""{element_context}""'),
                            ],
                        ),
                        CallStatement(
                            f"{sequence_type}.Update",
                            [Variable(sequence_context), Variable(element_context)],
                        ),
                        PragmaStatement(
                            "Warnings",
                            [
                                Variable("On"),
                                String(f'unused assignment to ""{element_context}""'),
                            ],
                        ),
                    ],
                ),
            ]

    assert False, f'unexpected statement type "{type(action)}"'


def session_substitution(session: model.Session) -> Callable[[expr.Expr], expr.Expr]:
    def func(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.And):
            return expr.AndThen(*expression.terms)
        if isinstance(expression, expr.Or):
            return expr.OrElse(*expression.terms)
        if isinstance(expression, expr.Selected):
            assert isinstance(expression.prefix, expr.Variable)
            assert isinstance(expression.prefix.type_, rty.Message)
            return expr.Call(
                f"{flat_name(expression.prefix.type_.name)}" * ID(f"Get_{expression.selector}"),
                [expr.Variable(f"{expression.prefix}_Ctx")],
            )
        if isinstance(expression, expr.Valid):
            if isinstance(expression.prefix, expr.Variable):
                assert isinstance(expression.prefix.type_, rty.Message)
                return expr.Call(
                    f"{flat_name(expression.prefix.type_.name)}" * ID("Structural_Valid_Message"),
                    [expr.Variable(f"{expression.prefix}_Ctx")],
                )
            if isinstance(expression.prefix, expr.Selected):
                assert isinstance(expression.prefix.prefix.type_, rty.Message)
                type_name = ID(flat_name(expression.prefix.prefix.type_.name))
                return expr.Call(
                    type_name * ID("Valid"),
                    [
                        expr.Variable(f"{expression.prefix.prefix}_Ctx"),
                        expr.Variable(type_name * f"F_{expression.prefix.selector}"),
                    ],
                )
        if isinstance(expression, expr.Aggregate):
            if len(expression.elements) == 1:
                return expr.NamedAggregate(
                    (
                        str(expr.First(const.TYPES_INDEX)),
                        expr.Val(const.TYPES_BYTE, expression.elements[0]),
                    )
                )
            return expr.Aggregate(*[expr.Val(const.TYPES_BYTE, e) for e in expression.elements])

        return expression

    return func


def valid_fields_condition(condition: expr.Expr) -> expr.Expr:
    return expr.And(
        *[expr.Valid(e) for e in condition.findall(lambda x: isinstance(x, expr.Selected))]
    )


def if_valid_fields(
    condition: expr.Expr, statements: Sequence[Statement], session: model.Session
) -> IfStatement:
    # ISSUE: Componlit/RecordFlux#569
    return IfStatement(
        # ensure validity of references message fields
        [
            (
                valid_fields_condition(condition)
                .substituted(session_substitution(session))
                .ada_expr(),
                statements,
            )
        ],
        [
            Assignment(
                "RFLX_Exception",
                TRUE,
            )
        ],
    )


def if_sufficient_space(
    required_space: expr.Expr, target_type: ID, target_context: ID, statements: Sequence[Statement]
) -> IfStatement:
    return IfStatement(
        [
            (
                And(
                    Call(
                        f"{target_type}.Has_Element",
                        [Variable(target_context)],
                    ),
                    GreaterEqual(
                        Call(
                            f"{target_type}.Available_Space",
                            [Variable(target_context)],
                        ),
                        required_space.ada_expr(),
                    ),
                ),
                statements,
            )
        ],
        [
            Assignment(
                "RFLX_Exception",
                TRUE,
            )
        ],
    )
