from typing import Mapping, Sequence

from rflx.ada import (Assignment, CallStatement, CaseStatement, ExpressionFunctionDeclaration,
                      FormalSubprogramDeclaration, FunctionSpecification,
                      GenericProcedureInstantiation, InOutParameter, NullStatement,
                      ObjectDeclaration, OutParameter, Parameter, Postcondition, PragmaStatement,
                      Precondition, ProcedureSpecification, Subprogram, SubprogramBody,
                      SubprogramDeclaration, UnitPart)
from rflx.expression import (TRUE, Add, Aggregate, And, AndThen, Call, Constrained, Div, Equal,
                             Expr, ForAllIn, GreaterEqual, If, In, Indexed, Last, LessEqual, Mod,
                             Name, NamedAggregate, Not, Number, Old, Or, Range, Selected, Slice,
                             Sub, Variable)
from rflx.model import FINAL, Enumeration, Field, Message, Payload, Scalar, Type

from .common import VALID_CONTEXT, GeneratorCommon, length_dependent_condition
from .types import Types


class GeneratorGenerator:
    def __init__(self, prefix: str = "") -> None:
        self.prefix = prefix
        self.types = Types(prefix)
        self.common = GeneratorCommon(prefix)

    def insert_function(self, type_name: str) -> Subprogram:
        return GenericProcedureInstantiation(
            "Insert",
            ProcedureSpecification(
                f"{self.types.types}.Insert",
                [
                    Parameter(["Value"], type_name),
                    InOutParameter(["Buffer"], self.types.bytes),
                    Parameter(["Offset"], self.types.offset),
                ],
            ),
            [self.types.index, self.types.byte, self.types.bytes, self.types.offset, type_name],
        )

    def create_internal_functions(
        self, message: Message, scalar_fields: Mapping[Field, Scalar]
    ) -> UnitPart:
        return UnitPart(
            [],
            [
                SubprogramBody(
                    ProcedureSpecification(
                        "Set_Field_Value",
                        [
                            InOutParameter(["Ctx"], "Context"),
                            Parameter(["Value"], "Field_Dependent_Value"),
                            OutParameter(["First", "Last"], self.types.bit_index),
                        ],
                    ),
                    [
                        ObjectDeclaration(
                            ["F"],
                            self.types.bit_index,
                            Call("Field_First", [Name("Ctx"), Selected("Value", "Fld")]),
                            True,
                        ),
                        ObjectDeclaration(
                            ["L"],
                            self.types.bit_index,
                            Call("Field_Last", [Name("Ctx"), Selected("Value", "Fld")]),
                            True,
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Buffer_First", self.types.index),
                            Call(self.types.byte_index, [Name("F")]),
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Buffer_Last", self.types.index),
                            Call(self.types.byte_index, [Name("L")]),
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Offset", self.types.offset),
                            Call(
                                self.types.offset,
                                [Mod(Sub(Number(8), Mod(Name("L"), Number(8))), Number(8))],
                            ),
                        ),
                    ],
                    [
                        Assignment("First", Name("F")),
                        Assignment("Last", Name("L")),
                        CaseStatement(
                            Selected("Value", "Fld"),
                            [
                                (
                                    Name(f.affixed_name),
                                    [
                                        CallStatement(
                                            "Insert",
                                            [
                                                Selected("Value", f"{f.name}_Value"),
                                                Slice(
                                                    Selected(Selected("Ctx", "Buffer"), "all"),
                                                    Name("Buffer_First"),
                                                    Name("Buffer_Last"),
                                                ),
                                                Name("Offset"),
                                            ],
                                        )
                                        if f in scalar_fields
                                        else NullStatement()
                                    ],
                                )
                                for f in message.all_fields
                            ],
                        ),
                    ],
                    [
                        Precondition(
                            AndThen(
                                Not(Constrained("Ctx")),
                                Call("Has_Buffer", [Name("Ctx")]),
                                In(Selected("Value", "Fld"), Range("Field")),
                                Call("Valid_Next", [Name("Ctx"), Selected("Value", "Fld")]),
                                GreaterEqual(
                                    Call(
                                        "Available_Space", [Name("Ctx"), Selected("Value", "Fld")],
                                    ),
                                    Call("Field_Length", [Name("Ctx"), Selected("Value", "Fld")]),
                                ),
                                ForAllIn(
                                    "F",
                                    Range("Field"),
                                    If(
                                        [
                                            (
                                                Call(
                                                    "Structural_Valid",
                                                    [
                                                        Indexed(
                                                            Selected("Ctx", "Cursors"), Name("F"),
                                                        )
                                                    ],
                                                ),
                                                LessEqual(
                                                    Selected(
                                                        Indexed(
                                                            Selected("Ctx", "Cursors"), Name("F"),
                                                        ),
                                                        "Last",
                                                    ),
                                                    Call(
                                                        "Field_Last",
                                                        [Name("Ctx"), Selected("Value", "Fld")],
                                                    ),
                                                ),
                                            )
                                        ]
                                    ),
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                Call("Has_Buffer", [Name("Ctx")]),
                                Equal(
                                    Name("First"),
                                    Call("Field_First", [Name("Ctx"), Selected("Value", "Fld")]),
                                ),
                                Equal(
                                    Name("Last"),
                                    Call("Field_Last", [Name("Ctx"), Selected("Value", "Fld")]),
                                ),
                                GreaterEqual(Name("First"), Selected("Ctx", "First")),
                                LessEqual(Name("First"), Add(Name("Last"), Number(1))),
                                LessEqual(
                                    Call(self.types.byte_index, [Name("Last")]),
                                    Selected("Ctx", "Buffer_Last"),
                                ),
                                ForAllIn(
                                    "F",
                                    Range("Field"),
                                    If(
                                        [
                                            (
                                                Call(
                                                    "Structural_Valid",
                                                    [
                                                        Indexed(
                                                            Selected("Ctx", "Cursors"), Name("F"),
                                                        )
                                                    ],
                                                ),
                                                LessEqual(
                                                    Selected(
                                                        Indexed(
                                                            Selected("Ctx", "Cursors"), Name("F"),
                                                        ),
                                                        "Last",
                                                    ),
                                                    Name("Last"),
                                                ),
                                            )
                                        ]
                                    ),
                                ),
                                *[
                                    Equal(e, Old(e))
                                    for e in [
                                        Selected("Ctx", "Buffer_First"),
                                        Selected("Ctx", "Buffer_Last"),
                                        Selected("Ctx", "First"),
                                        Selected("Ctx", "Cursors"),
                                    ]
                                ],
                            )
                        ),
                    ],
                )
            ],
        )

    def create_scalar_setter_procedures(
        self, message: Message, scalar_fields: Mapping[Field, Scalar]
    ) -> UnitPart:
        def specification(field: Field, field_type: Type) -> ProcedureSpecification:
            type_name = (
                field_type.enum_name
                if isinstance(field_type, Enumeration) and field_type.always_valid
                else field_type.name
            )
            return ProcedureSpecification(
                f"Set_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Value"], type_name)],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f, t),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                Call(
                                    "Field_Condition",
                                    [
                                        Name("Ctx"),
                                        Aggregate(
                                            Name(f.affixed_name),
                                            Name("Value")
                                            if not isinstance(t, Enumeration)
                                            else Call("Convert", [Name("Value")]),
                                        ),
                                    ],
                                ),
                                Call("Valid", [Name("Value")])
                                if not isinstance(t, Enumeration)
                                else TRUE,
                                GreaterEqual(
                                    Call("Available_Space", [Name("Ctx"), Name(f.affixed_name)]),
                                    Call("Field_Length", [Name("Ctx"), Name(f.affixed_name)]),
                                ),
                            )
                        ),
                        Postcondition(
                            And(
                                VALID_CONTEXT,
                                Call("Has_Buffer", [Name("Ctx")]),
                                Call("Valid", [Name("Ctx"), Name(f.affixed_name)]),
                                Equal(
                                    Call(f"Get_{f.name}", [Name("Ctx")]),
                                    Aggregate(TRUE, Name("Value"))
                                    if isinstance(t, Enumeration) and t.always_valid
                                    else Name("Value"),
                                ),
                                *self.setter_postconditions(message, f, t),
                                *[
                                    Equal(
                                        Call("Cursor", [Name("Ctx"), Name(p.affixed_name)]),
                                        Old(Call("Cursor", [Name("Ctx"), Name(p.affixed_name)])),
                                    )
                                    for p in message.predecessors(f)
                                ],
                            )
                        ),
                    ],
                )
                for f, t in scalar_fields.items()
            ],
            [
                SubprogramBody(
                    specification(f, t),
                    [
                        ObjectDeclaration(
                            ["Field_Value"],
                            "Field_Dependent_Value",
                            Aggregate(
                                Name(f.affixed_name),
                                Name("Value")
                                if not isinstance(t, Enumeration)
                                else Call("Convert", [Name("Value")]),
                            ),
                            True,
                        ),
                        ObjectDeclaration(["First", "Last"], self.types.bit_index),
                    ],
                    [
                        CallStatement(
                            "Reset_Dependent_Fields", [Name("Ctx"), Name(f.affixed_name)],
                        ),
                        CallStatement(
                            "Set_Field_Value",
                            [Name("Ctx"), Name("Field_Value"), Name("First"), Name("Last")],
                        ),
                        Assignment(
                            "Ctx",
                            Aggregate(
                                Selected("Ctx", "Buffer_First"),
                                Selected("Ctx", "Buffer_Last"),
                                Selected("Ctx", "First"),
                                Name("Last"),
                                Selected("Ctx", "Buffer"),
                                Selected("Ctx", "Cursors"),
                            ),
                        ),
                        Assignment(
                            Indexed(Selected("Ctx", "Cursors"), Name(f.affixed_name)),
                            NamedAggregate(
                                ("State", Name("S_Valid")),
                                ("First", Name("First")),
                                ("Last", Name("Last")),
                                ("Value", Name("Field_Value")),
                                (
                                    "Predecessor",
                                    Selected(
                                        Indexed(Selected("Ctx", "Cursors"), Name(f.affixed_name)),
                                        "Predecessor",
                                    ),
                                ),
                            ),
                        ),
                        Assignment(
                            Indexed(
                                Selected("Ctx", "Cursors"),
                                Call("Successor", [Name("Ctx"), Name(f.affixed_name)]),
                            ),
                            NamedAggregate(
                                ("State", Name("S_Invalid")), ("Predecessor", Name(f.affixed_name)),
                            ),
                        ),
                    ],
                )
                for f, t in scalar_fields.items()
            ],
        )

    def create_composite_setter_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(f"Set_{field.name}", [InOutParameter(["Ctx"], "Context")])

        def specification_bounded(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Set_Bounded_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Length"], self.types.bit_length)],
            )

        formal_parameters = [
            FormalSubprogramDeclaration(
                ProcedureSpecification(
                    "Process_Payload", [OutParameter(["Payload"], self.types.bytes)],
                )
            )
        ]

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.unbounded_composite_setter_preconditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(message, f, message.types[f]),
                            )
                        ),
                    ],
                    formal_parameters,
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and unbounded_setter_required(message, f)
            ]
            + [
                SubprogramDeclaration(
                    specification_bounded(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.bounded_composite_setter_preconditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(message, f, message.types[f]),
                            )
                        ),
                    ],
                    formal_parameters,
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and bounded_setter_required(message, f)
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            self.types.bit_index,
                            Call("Field_First", [Name("Ctx"), Name(f.affixed_name)]),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.bit_index,
                            Call("Field_Last", [Name("Ctx"), Name(f.affixed_name)]),
                            True,
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Buffer_First", self.types.index),
                            Call(self.types.byte_index, [Name("First")]),
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Buffer_Last", self.types.index),
                            Call(self.types.byte_index, [Name("Last")]),
                        ),
                    ],
                    [
                        CallStatement(f"Initialize_{f.name}", [Name("Ctx")]),
                        CallStatement(
                            "Process_Payload",
                            [
                                Slice(
                                    Selected(Selected("Ctx", "Buffer"), "all"),
                                    Name("Buffer_First"),
                                    Name("Buffer_Last"),
                                ),
                            ],
                        ),
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and unbounded_setter_required(message, f)
            ]
            + [
                SubprogramBody(
                    specification_bounded(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            self.types.bit_index,
                            Call("Field_First", [Name("Ctx"), Name(f.affixed_name)]),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.bit_index,
                            Add(Name("First"), Name("Length"), -Number(1)),
                            True,
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Buffer_First", self.types.index),
                            Call(self.types.byte_index, [Name("First")]),
                        ),
                        ExpressionFunctionDeclaration(
                            FunctionSpecification("Buffer_Last", self.types.index),
                            Call(self.types.byte_index, [Name("Last")]),
                        ),
                    ],
                    [
                        CallStatement(
                            f"Initialize_Bounded_{f.name}", [Name("Ctx"), Name("Length")]
                        ),
                        CallStatement(
                            "Process_Payload",
                            [
                                Slice(
                                    Selected(Selected("Ctx", "Buffer"), "all"),
                                    Name("Buffer_First"),
                                    Name("Buffer_Last"),
                                ),
                            ],
                        ),
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and bounded_setter_required(message, f)
            ],
        )

    def create_composite_initialize_procedures(self, message: Message) -> UnitPart:
        def specification(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_{field.name}", [InOutParameter(["Ctx"], "Context")]
            )

        def specification_bounded(field: Field) -> ProcedureSpecification:
            return ProcedureSpecification(
                f"Initialize_Bounded_{field.name}",
                [InOutParameter(["Ctx"], "Context"), Parameter(["Length"], self.types.bit_length)],
            )

        return UnitPart(
            [
                SubprogramDeclaration(
                    specification(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.unbounded_composite_setter_preconditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(message, f, message.types[f]),
                            )
                        ),
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and unbounded_setter_required(message, f)
            ]
            + [
                SubprogramDeclaration(
                    specification_bounded(f),
                    [
                        Precondition(
                            AndThen(
                                *self.setter_preconditions(f),
                                *self.bounded_composite_setter_preconditions(message, f),
                            )
                        ),
                        Postcondition(
                            And(
                                *self.composite_setter_postconditions(message, f, message.types[f]),
                            )
                        ),
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and bounded_setter_required(message, f)
            ],
            [
                SubprogramBody(
                    specification(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            self.types.bit_index,
                            Call("Field_First", [Name("Ctx"), Name(f.affixed_name)]),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.bit_index,
                            Call("Field_Last", [Name("Ctx"), Name(f.affixed_name)]),
                            True,
                        ),
                    ],
                    [
                        CallStatement(
                            "Reset_Dependent_Fields", [Name("Ctx"), Name(f.affixed_name)]
                        ),
                        Assignment(
                            "Ctx",
                            Aggregate(
                                Selected("Ctx", "Buffer_First"),
                                Selected("Ctx", "Buffer_Last"),
                                Selected("Ctx", "First"),
                                Name("Last"),
                                Selected("Ctx", "Buffer"),
                                Selected("Ctx", "Cursors"),
                            ),
                        ),
                        # WORKAROUND:
                        # Limitation of GNAT Community 2019 / SPARK Pro 20.0
                        # Provability of predicate is increased by adding part of
                        # predicate as assert
                        PragmaStatement(
                            "Assert",
                            [str(self.common.message_structure_invariant(message, prefix=True))],
                        ),
                        Assignment(
                            Indexed(Selected("Ctx", "Cursors"), Name(f.affixed_name)),
                            NamedAggregate(
                                ("State", Name("S_Structural_Valid")),
                                ("First", Name("First")),
                                ("Last", Name("Last")),
                                ("Value", NamedAggregate(("Fld", Name(f.affixed_name)))),
                                (
                                    "Predecessor",
                                    Selected(
                                        Indexed(Selected("Ctx", "Cursors"), Name(f.affixed_name)),
                                        "Predecessor",
                                    ),
                                ),
                            ),
                        ),
                        Assignment(
                            Indexed(
                                Selected("Ctx", "Cursors"),
                                Call("Successor", [Name("Ctx"), Name(f.affixed_name)]),
                            ),
                            NamedAggregate(
                                ("State", Name("S_Invalid")), ("Predecessor", Name(f.affixed_name)),
                            ),
                        ),
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and unbounded_setter_required(message, f)
            ]
            + [
                SubprogramBody(
                    specification_bounded(f),
                    [
                        ObjectDeclaration(
                            ["First"],
                            self.types.bit_index,
                            Call("Field_First", [Name("Ctx"), Name(f.affixed_name)]),
                            True,
                        ),
                        ObjectDeclaration(
                            ["Last"],
                            self.types.bit_index,
                            Add(Name("First"), Name("Length"), -Number(1)),
                            True,
                        ),
                    ],
                    [
                        CallStatement(
                            "Reset_Dependent_Fields", [Name("Ctx"), Name(f.affixed_name)],
                        ),
                        Assignment(
                            "Ctx",
                            Aggregate(
                                Selected("Ctx", "Buffer_First"),
                                Selected("Ctx", "Buffer_Last"),
                                Selected("Ctx", "First"),
                                Name("Last"),
                                Selected("Ctx", "Buffer"),
                                Selected("Ctx", "Cursors"),
                            ),
                        ),
                        # WORKAROUND:
                        # Limitation of GNAT Community 2019 / SPARK Pro 20.0
                        # Provability of predicate is increased by adding part of
                        # predicate as assert
                        PragmaStatement(
                            "Assert",
                            [str(self.common.message_structure_invariant(message, prefix=True))],
                        ),
                        Assignment(
                            Indexed(Selected("Ctx", "Cursors"), Name(f.affixed_name)),
                            NamedAggregate(
                                ("State", Name("S_Structural_Valid")),
                                ("First", Name("First")),
                                ("Last", Name("Last")),
                                ("Value", NamedAggregate(("Fld", Name(f.affixed_name)))),
                                (
                                    "Predecessor",
                                    Selected(
                                        Indexed(Selected("Ctx", "Cursors"), Name(f.affixed_name)),
                                        "Predecessor",
                                    ),
                                ),
                            ),
                        ),
                        Assignment(
                            Indexed(
                                Selected("Ctx", "Cursors"),
                                Call("Successor", [Name("Ctx"), Name(f.affixed_name)]),
                            ),
                            NamedAggregate(
                                ("State", Name("S_Invalid")), ("Predecessor", Name(f.affixed_name)),
                            ),
                        ),
                    ],
                )
                for f, t in message.types.items()
                if isinstance(t, Payload) and bounded_setter_required(message, f)
            ],
        )

    def setter_preconditions(self, field: Field) -> Sequence[Expr]:
        return [
            VALID_CONTEXT,
            Not(Constrained("Ctx")),
            Call("Has_Buffer", [Name("Ctx")]),
            Call("Valid_Next", [Name("Ctx"), Name(field.affixed_name)]),
            LessEqual(
                Call("Field_Last", [Name("Ctx"), Name(field.affixed_name)]),
                Div(Last(self.types.bit_index), Number(2)),
            ),
        ]

    def setter_postconditions(
        self, message: Message, field: Field, field_type: Type
    ) -> Sequence[Expr]:
        return [
            *[
                Call("Invalid", [Name("Ctx"), Name(p.affixed_name)])
                for p in message.successors(field)
                if p != FINAL
            ],
            *self.common.valid_path_to_next_field_condition(message, field, field_type),
            *[
                Equal(e, Old(e))
                for e in [
                    Selected("Ctx", "Buffer_First"),
                    Selected("Ctx", "Buffer_Last"),
                    Selected("Ctx", "First"),
                    Call("Predecessor", [Name("Ctx"), Name(field.affixed_name)]),
                    Call("Valid_Next", [Name("Ctx"), Name(field.affixed_name)]),
                ]
                + [
                    Call(f"Get_{p.name}", [Name("Ctx")])
                    for p in message.definite_predecessors(field)
                    if isinstance(message.types[p], Scalar)
                ]
            ],
        ]

    def composite_setter_postconditions(
        self, message: Message, field: Field, field_type: Type
    ) -> Sequence[Expr]:
        return [
            VALID_CONTEXT,
            Call("Has_Buffer", [Name("Ctx")]),
            *self.setter_postconditions(message, field, field_type),
            Call("Structural_Valid", [Name("Ctx"), Name(field.affixed_name)]),
        ]

    @staticmethod
    def unbounded_composite_setter_preconditions(message: Message, field: Field) -> Sequence[Expr]:
        return [
            Call(
                "Field_Condition",
                [Name("Ctx"), NamedAggregate(("Fld", Name(field.affixed_name)))]
                + (
                    [Call("Field_Length", [Name("Ctx"), Name(field.affixed_name)],)]
                    if length_dependent_condition(message)
                    else []
                ),
            ),
            GreaterEqual(
                Call("Available_Space", [Name("Ctx"), Name(field.affixed_name)]),
                Call("Field_Length", [Name("Ctx"), Name(field.affixed_name)],),
            ),
        ]

    def bounded_composite_setter_preconditions(
        self, message: Message, field: Field
    ) -> Sequence[Expr]:
        return [
            Call(
                "Field_Condition",
                [Name("Ctx"), NamedAggregate(("Fld", Name(field.affixed_name)))]
                + ([Name("Length")] if length_dependent_condition(message) else []),
            ),
            GreaterEqual(
                Call("Available_Space", [Name("Ctx"), Name(field.affixed_name)]), Name("Length"),
            ),
            LessEqual(
                Add(Call("Field_First", [Name("Ctx"), Name(field.affixed_name)]), Name("Length"),),
                Div(Last(self.types.bit_index), Number(2)),
            ),
            Or(
                *[
                    And(
                        *[
                            Call("Valid", [Name("Ctx"), Name(field.affixed_name)])
                            for field in message.fields
                            if Variable(field.name) in l.condition.variables()
                        ],
                        l.condition.simplified(
                            {
                                Variable(field.name): Call(f"Get_{field.name}", [Name("Ctx")])
                                for field in message.fields
                                if Variable(field.name) in l.condition.variables()
                            }
                        ),
                    )
                    for l in message.incoming(field)
                    if Last("Message") in l.length
                ]
            ),
        ]


def unbounded_setter_required(message: Message, field: Field) -> bool:
    return any(True for l in message.incoming(field) if Last("Message") not in l.length)


def bounded_setter_required(message: Message, field: Field) -> bool:
    return any(True for l in message.incoming(field) if Last("Message") in l.length)
