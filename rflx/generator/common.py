from typing import Callable, Mapping, Optional, Sequence

import rflx.ada as ada
import rflx.expression as expr
from rflx.const import BUILTINS_PACKAGE
from rflx.model import (
    FINAL,
    INITIAL,
    Composite,
    Enumeration,
    Field,
    Link,
    Message,
    ModularInteger,
    Scalar,
    Type,
    is_builtin_type,
)

from . import const


def substitution(
    message: Message,
    embedded: bool = False,
    public: bool = False,
    target_type: Optional[ada.ID] = const.TYPES_U64,
) -> Callable[[expr.Expr], expr.Expr]:
    facts = substitution_facts(message, embedded, public, target_type)

    def func(expression: expr.Expr) -> expr.Expr:
        def byte_aggregate(aggregate: expr.Aggregate) -> expr.Aggregate:
            return expr.Aggregate(*[expr.Val(const.TYPES_BYTE, e) for e in aggregate.elements])

        if isinstance(expression, expr.Name) and expression in facts:
            return facts[expression]

        if isinstance(expression, (expr.Equal, expr.NotEqual)):
            field = None
            aggregate = None
            if isinstance(expression.left, expr.Variable) and isinstance(
                expression.right, expr.Aggregate
            ):
                field = Field(expression.left.name)
                aggregate = byte_aggregate(expression.right)
            elif isinstance(expression.left, expr.Aggregate) and isinstance(
                expression.right, expr.Variable
            ):
                field = Field(expression.right.name)
                aggregate = byte_aggregate(expression.left)
            if field and aggregate:
                assert field in message.fields
                if embedded:
                    return expr.Equal(
                        expr.Indexed(
                            expr.Variable(expr.ID("Buffer") * "all"),
                            expr.ValueRange(
                                expr.Call(
                                    const.TYPES_BYTE_INDEX,
                                    [
                                        expr.Selected(
                                            expr.Indexed(
                                                expr.Variable("Cursors"),
                                                expr.Variable(field.affixed_name),
                                            ),
                                            "First",
                                        )
                                    ],
                                ),
                                expr.Call(
                                    const.TYPES_BYTE_INDEX,
                                    [
                                        expr.Selected(
                                            expr.Indexed(
                                                expr.Variable("Cursors"),
                                                expr.Variable(field.affixed_name),
                                            ),
                                            "Last",
                                        )
                                    ],
                                ),
                            ),
                        ),
                        aggregate,
                    )
                equal_call = expr.Call(
                    "Equal",
                    [expr.Variable("Ctx"), expr.Variable(field.affixed_name), aggregate],
                )
                return equal_call if isinstance(expression, expr.Equal) else expr.Not(equal_call)

        def field_value(field: Field) -> expr.Expr:
            if public:
                return expr.Call(f"Get_{field.name}", [expr.Variable("Ctx")])
            return expr.Selected(
                expr.Indexed(
                    expr.Variable(expr.ID("Ctx") * "Cursors" if not embedded else "Cursors"),
                    expr.Variable(field.affixed_name),
                ),
                expr.ID("Value") * f"{field.name}_Value",
            )

        if isinstance(expression, expr.Relation):
            if (
                isinstance(expression.left, expr.Variable)
                and Field(expression.left.name) in message.fields
                and isinstance(expression.right, expr.Number)
            ):
                return expression.__class__(
                    field_value(Field(expression.left.name)), expression.right
                )
            if (
                isinstance(expression.right, expr.Variable)
                and Field(expression.right.name) in message.fields
                and isinstance(expression.left, expr.Number)
            ):
                return expression.__class__(
                    expression.left, field_value(Field(expression.right.name))
                )

        return expression

    return func


def substitution_facts(
    message: Message,
    embedded: bool = False,
    public: bool = False,
    target_type: Optional[ada.ID] = const.TYPES_U64,
) -> Mapping[expr.Name, expr.Expr]:
    def prefixed(name: str) -> expr.Expr:
        return expr.Variable(expr.ID("Ctx") * name) if not embedded else expr.Variable(name)

    first = prefixed("First")
    last = prefixed("Last")
    cursors = prefixed("Cursors")

    def field_first(field: Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_First", [expr.Variable("Ctx"), expr.Variable(field.affixed_name)]
            )
        return expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "First")

    def field_last(field: Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_Last", [expr.Variable("Ctx"), expr.Variable(field.affixed_name)]
            )
        return expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "Last")

    def field_size(field: Field) -> expr.Expr:
        if public:
            return expr.Call(
                "Field_Size", [expr.Variable("Ctx"), expr.Variable(field.affixed_name)]
            )
        return expr.Add(
            expr.Sub(
                expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "Last"),
                expr.Selected(expr.Indexed(cursors, expr.Variable(field.affixed_name)), "First"),
            ),
            expr.Number(1),
        )

    def field_value(field: Field, field_type: Type) -> expr.Expr:
        if isinstance(field_type, Enumeration):
            if public:
                return expr.Call(
                    "To_Base", [expr.Call(f"Get_{field.name}", [expr.Variable("Ctx")])]
                )
            return expr.Selected(
                expr.Indexed(cursors, expr.Variable(field.affixed_name)),
                expr.ID("Value") * f"{field.name}_Value",
            )
        if isinstance(field_type, Scalar):
            if public:
                return expr.Call(f"Get_{field.name}", [expr.Variable("Ctx")])
            return expr.Selected(
                expr.Indexed(cursors, expr.Variable(field.affixed_name)),
                expr.ID("Value") * f"{field.name}_Value",
            )
        if isinstance(field_type, Composite):
            return expr.Variable(field.name)

        assert False, f'unexpected type "{type(field_type).__name__}"'

    def type_conversion(expression: expr.Expr) -> expr.Expr:
        return expr.Call(target_type, [expression]) if target_type else expression

    return {
        **{expr.First("Message"): type_conversion(first)},
        **{expr.Last("Message"): type_conversion(last)},
        **{expr.Size("Message"): type_conversion(expr.Add(last, -first, expr.Number(1)))},
        **{expr.First(f.name): type_conversion(field_first(f)) for f in message.fields},
        **{expr.Last(f.name): type_conversion(field_last(f)) for f in message.fields},
        **{expr.Size(f.name): type_conversion(field_size(f)) for f in message.fields},
        **{
            expr.Variable(f.name): type_conversion(field_value(f, t))
            for f, t in message.types.items()
        },
        **{
            expr.Variable(l): type_conversion(expr.Call("To_Base", [expr.Variable(l)]))
            for t in message.types.values()
            if isinstance(t, Enumeration)
            for l in t.literals.keys()
        },
        **{
            expr.Variable(t.package * l): type_conversion(
                expr.Call("To_Base", [expr.Variable(t.package * l)])
            )
            for t in message.types.values()
            if isinstance(t, Enumeration)
            for l in t.literals.keys()
        },
        # ISSUE: Componolit/RecordFlux#276
        **{expr.ValidChecksum(f): expr.TRUE for f in message.checksums},
    }


def message_structure_invariant(
    message: Message, prefix: str, link: Link = None, embedded: bool = False
) -> ada.Expr:
    def prefixed(name: str) -> expr.Expr:
        return expr.Selected(expr.Variable("Ctx"), name) if not embedded else expr.Variable(name)

    if not link:
        return message_structure_invariant(message, prefix, message.outgoing(INITIAL)[0], embedded)

    source = link.source
    target = link.target

    if target is FINAL:
        return ada.TRUE

    field_type = message.types[target]
    condition = link.condition.substituted(substitution(message, embedded)).simplified()
    size = (
        expr.Size(prefix * full_base_type_name(field_type))
        if isinstance(field_type, Scalar)
        else link.size.substituted(
            substitution(message, embedded, target_type=const.TYPES_BIT_LENGTH)
        ).simplified()
    )
    first = (
        prefixed("First")
        if source == INITIAL
        else link.first.substituted(
            substitution(message, embedded, target_type=const.TYPES_BIT_INDEX)
        )
        .substituted(
            mapping={
                expr.UNDEFINED: expr.Add(
                    expr.Selected(
                        expr.Indexed(prefixed("Cursors"), expr.Variable(source.affixed_name)),
                        "Last",
                    ),
                    expr.Number(1),
                )
            }
        )
        .simplified()
    )
    invariant = [
        message_structure_invariant(message, prefix, l, embedded) for l in message.outgoing(target)
    ]

    return ada.If(
        [
            (
                ada.AndThen(
                    ada.Call(
                        "Structural_Valid",
                        [
                            ada.Indexed(
                                prefixed("Cursors").ada_expr(), ada.Variable(target.affixed_name)
                            )
                        ],
                    ),
                    *([condition.ada_expr()] if condition != expr.TRUE else []),
                ),
                ada.AndThen(
                    ada.Equal(
                        ada.Add(
                            ada.Sub(
                                ada.Selected(
                                    ada.Indexed(
                                        prefixed("Cursors").ada_expr(),
                                        ada.Variable(target.affixed_name),
                                    ),
                                    "Last",
                                ),
                                ada.Selected(
                                    ada.Indexed(
                                        prefixed("Cursors").ada_expr(),
                                        ada.Variable(target.affixed_name),
                                    ),
                                    "First",
                                ),
                            ),
                            ada.Number(1),
                        ),
                        size.ada_expr(),
                    ),
                    ada.Equal(
                        ada.Selected(
                            ada.Indexed(
                                prefixed("Cursors").ada_expr(),
                                ada.Variable(target.affixed_name),
                            ),
                            "Predecessor",
                        ),
                        ada.Variable(source.affixed_name),
                    ),
                    ada.Equal(
                        ada.Selected(
                            ada.Indexed(
                                prefixed("Cursors").ada_expr(),
                                ada.Variable(target.affixed_name),
                            ),
                            "First",
                        ),
                        first.ada_expr(),
                    ),
                    *[i for i in invariant if i != ada.TRUE],
                ),
            )
        ]
    )


def context_predicate(message: Message, composite_fields: Sequence[Field], prefix: str) -> ada.Expr:
    def valid_predecessors_invariant() -> ada.Expr:
        return ada.AndThen(
            *[
                ada.If(
                    [
                        (
                            ada.Call(
                                "Structural_Valid",
                                [
                                    ada.Indexed(
                                        ada.Variable("Cursors"),
                                        ada.Variable(f.affixed_name),
                                    )
                                ],
                            ),
                            ada.Or(
                                *[
                                    expr.AndThen(
                                        expr.Call(
                                            "Structural_Valid"
                                            if l.source in composite_fields
                                            else "Valid",
                                            [
                                                expr.Indexed(
                                                    expr.Variable("Cursors"),
                                                    expr.Variable(l.source.affixed_name),
                                                )
                                            ],
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Indexed(
                                                    expr.Variable("Cursors"),
                                                    expr.Variable(f.affixed_name),
                                                ),
                                                "Predecessor",
                                            ),
                                            expr.Variable(l.source.affixed_name),
                                        ),
                                        l.condition.substituted(
                                            substitution(message, embedded=True)
                                        ),
                                    )
                                    .simplified()
                                    .ada_expr()
                                    for l in message.incoming(f)
                                ]
                            ),
                        )
                    ]
                )
                for f in message.fields
                if f not in message.direct_successors(INITIAL)
            ]
        )

    def invalid_successors_invariant() -> ada.Expr:
        return ada.AndThen(
            *[
                ada.If(
                    [
                        (
                            ada.AndThen(
                                *[
                                    ada.Call(
                                        "Invalid",
                                        [
                                            ada.Indexed(
                                                ada.Variable("Cursors"),
                                                ada.Variable(p.affixed_name),
                                            )
                                        ],
                                    )
                                    for p in message.direct_predecessors(f)
                                ]
                            ),
                            ada.Call(
                                "Invalid",
                                [
                                    ada.Indexed(
                                        ada.Variable("Cursors"),
                                        ada.Variable(f.affixed_name),
                                    )
                                ],
                            ),
                        )
                    ]
                )
                for f in message.fields
                if f not in message.direct_successors(INITIAL)
            ]
        )

    return ada.AndThen(
        ada.If(
            [
                (
                    ada.NotEqual(ada.Variable("Buffer"), ada.Variable("null")),
                    ada.And(
                        ada.Equal(ada.First("Buffer"), ada.Variable("Buffer_First")),
                        ada.Equal(ada.Last("Buffer"), ada.Variable("Buffer_Last")),
                    ),
                )
            ]
        ),
        public_context_predicate(),
        ada.LessEqual(ada.Variable("First"), ada.Variable("Message_Last")),
        ada.LessEqual(ada.Variable("Message_Last"), ada.Variable("Last")),
        ada.ForAllIn(
            "F",
            ada.ValueRange(ada.First("Field"), ada.Last("Field")),
            ada.If(
                [
                    (
                        ada.Call(
                            "Structural_Valid",
                            [ada.Indexed(ada.Variable("Cursors"), ada.Variable("F"))],
                        ),
                        ada.And(
                            ada.GreaterEqual(
                                ada.Selected(
                                    ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")), "First"
                                ),
                                ada.Variable("First"),
                            ),
                            ada.LessEqual(
                                ada.Selected(
                                    ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")), "Last"
                                ),
                                ada.Variable("Message_Last"),
                            ),
                            ada.LessEqual(
                                ada.Selected(
                                    ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")), "First"
                                ),
                                ada.Add(
                                    ada.Selected(
                                        ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")),
                                        "Last",
                                    ),
                                    ada.Number(1),
                                ),
                            ),
                            ada.Equal(
                                ada.Selected(
                                    ada.Selected(
                                        ada.Indexed(ada.Variable("Cursors"), ada.Variable("F")),
                                        "Value",
                                    ),
                                    "Fld",
                                ),
                                ada.Variable("F"),
                            ),
                        ),
                    )
                ]
            ),
        ),
        valid_predecessors_invariant(),
        invalid_successors_invariant(),
        message_structure_invariant(message, prefix, embedded=True),
    )


def public_context_predicate() -> ada.Expr:
    return ada.And(
        ada.GreaterEqual(
            ada.Call(const.TYPES_BYTE_INDEX, [ada.Variable("First")]), ada.Variable("Buffer_First")
        ),
        ada.LessEqual(
            ada.Call(const.TYPES_BYTE_INDEX, [ada.Variable("Last")]), ada.Variable("Buffer_Last")
        ),
        ada.LessEqual(ada.Variable("First"), ada.Variable("Last")),
        ada.Less(ada.Variable("Last"), ada.Last(const.TYPES_BIT_INDEX)),
    )


def valid_path_to_next_field_condition(message: Message, field: Field) -> Sequence[ada.Expr]:
    return [
        ada.If(
            [
                (
                    l.condition.substituted(substitution(message, public=True))
                    .simplified()
                    .ada_expr(),
                    ada.And(
                        ada.Equal(
                            ada.Call(
                                "Predecessor",
                                [ada.Variable("Ctx"), ada.Variable(l.target.affixed_name)],
                            ),
                            ada.Variable(field.affixed_name),
                        ),
                        ada.Call(
                            "Valid_Next",
                            [ada.Variable("Ctx"), ada.Variable(l.target.affixed_name)],
                        )
                        if l.target != FINAL
                        else ada.TRUE,
                    ),
                )
            ]
        )
        for l in message.outgoing(field)
        if l.target != FINAL
    ]


def sufficient_space_for_field_condition(field_name: ada.Name) -> ada.Expr:
    return ada.GreaterEqual(
        ada.Call("Available_Space", [ada.Variable("Ctx"), field_name]),
        ada.Call("Field_Size", [ada.Variable("Ctx"), field_name]),
    )


def initialize_field_statements(
    message: Message, field: Field, prefix: str
) -> Sequence[ada.Statement]:
    return [
        ada.CallStatement(
            "Reset_Dependent_Fields",
            [ada.Variable("Ctx"), ada.Variable(field.affixed_name)],
        ),
        ada.Assignment("Ctx.Message_Last", ada.Variable("Last")),
        # WORKAROUND:
        # Limitation of GNAT Community 2019 / SPARK Pro 20.0
        # Provability of predicate is increased by adding part of
        # predicate as assert
        ada.PragmaStatement(
            "Assert",
            [message_structure_invariant(message, prefix)],
        ),
        ada.Assignment(
            ada.Indexed(ada.Variable("Ctx.Cursors"), ada.Variable(field.affixed_name)),
            ada.NamedAggregate(
                ("State", ada.Variable("S_Structural_Valid")),
                ("First", ada.Variable("First")),
                ("Last", ada.Variable("Last")),
                (
                    "Value",
                    ada.NamedAggregate(("Fld", ada.Variable(field.affixed_name))),
                ),
                (
                    "Predecessor",
                    ada.Selected(
                        ada.Indexed(
                            ada.Variable("Ctx.Cursors"),
                            ada.Variable(field.affixed_name),
                        ),
                        "Predecessor",
                    ),
                ),
            ),
        ),
        ada.Assignment(
            ada.Indexed(
                ada.Variable("Ctx.Cursors"),
                ada.Call(
                    "Successor",
                    [ada.Variable("Ctx"), ada.Variable(field.affixed_name)],
                ),
            ),
            ada.NamedAggregate(
                ("State", ada.Variable("S_Invalid")),
                ("Predecessor", ada.Variable(field.affixed_name)),
            ),
        ),
    ]


def field_bit_location_declarations(field_name: ada.Name) -> Sequence[ada.Declaration]:
    return [
        ada.ObjectDeclaration(
            ["First"],
            const.TYPES_BIT_INDEX,
            ada.Call("Field_First", [ada.Variable("Ctx"), field_name]),
            True,
        ),
        ada.ObjectDeclaration(
            ["Last"],
            const.TYPES_BIT_INDEX,
            ada.Call("Field_Last", [ada.Variable("Ctx"), field_name]),
            True,
        ),
    ]


def field_byte_location_declarations() -> Sequence[ada.Declaration]:
    return [
        ada.ExpressionFunctionDeclaration(
            ada.FunctionSpecification("Buffer_First", const.TYPES_INDEX),
            ada.Call(const.TYPES_BYTE_INDEX, [ada.Variable("First")]),
        ),
        ada.ExpressionFunctionDeclaration(
            ada.FunctionSpecification("Buffer_Last", const.TYPES_INDEX),
            ada.Call(const.TYPES_BYTE_INDEX, [ada.Variable("Last")]),
        ),
        ada.ExpressionFunctionDeclaration(
            ada.FunctionSpecification("Offset", const.TYPES_OFFSET),
            ada.Call(
                const.TYPES_OFFSET,
                [
                    ada.Mod(
                        ada.Sub(ada.Number(8), ada.Mod(ada.Variable("Last"), ada.Number(8))),
                        ada.Number(8),
                    )
                ],
            ),
        ),
    ]


def prefixed_type_identifier(type_identifier: ada.ID, prefix: str) -> ada.ID:
    if is_builtin_type(type_identifier):
        return type_identifier

    return prefix * type_identifier


def base_type_name(scalar_type: Scalar) -> ada.ID:
    if isinstance(scalar_type, ModularInteger):
        return ada.ID(scalar_type.name)

    return ada.ID(scalar_type.name + "_Base")


def full_base_type_name(scalar_type: Scalar) -> ada.ID:
    if scalar_type.package == BUILTINS_PACKAGE:
        return const.BUILTIN_TYPES_PACKAGE * scalar_type.name + "_Base"

    if isinstance(scalar_type, ModularInteger):
        return ada.ID(scalar_type.identifier)

    return ada.ID(scalar_type.identifier + "_Base")


def enum_name(enum_type: Enumeration) -> ada.ID:
    return ada.ID(enum_type.name + "_Enum")


def full_enum_name(enum_type: Enumeration) -> ada.ID:
    return ada.ID(enum_type.identifier + "_Enum")


def sequence_name(message: Message, field: Field) -> ada.ID:
    return ada.ID(message.types[field].name + "_Sequence")


def size_dependent_condition(message: Message) -> bool:
    return (
        len(
            [
                l
                for link in message.structure
                for l in link.condition.findall(lambda x: isinstance(x, expr.Size))
            ]
        )
        > 0
    )
