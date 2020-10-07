from typing import Any

import pytest

from rflx import declaration as decl, expression as expr, model, statement as stmt
from rflx.error import Location
from rflx.identifier import ID
from rflx.specification import ast, grammar, parser
from rflx.specification.parser import ParseFatalException


@pytest.mark.parametrize(
    "string,expected",
    [("X", ID("X")), ("X2", ID("X2")), ("X_Y", ID("X_Y")), ("X_Y_3", ID("X_Y_3"))],
)
def test_unqualified_identifier(string: str, expected: ID) -> None:
    actual = grammar.unqualified_identifier().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X", ID("X")),
        ("X2", ID("X2")),
        ("X_Y", ID("X_Y")),
        ("X_Y_3", ID("X_Y_3")),
        ("X::Y", ID("X::Y")),
        ("X2::Y2", ID("X2::Y2")),
        ("X_Y::Z", ID("X_Y::Z")),
        ("X_Y_3::Z_4", ID("X_Y_3::Z_4")),
        ("X::Y::Z", ID("X::Y::Z")),
    ],
)
def test_qualified_identifier(string: str, expected: ID) -> None:
    actual = grammar.qualified_identifier().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("1000", expr.Number(1000)),
        ("1_000", expr.Number(1000)),
        ("16#6664#", expr.Number(26212)),
        ("16#66_64#", expr.Number(26212)),
        ("-1000", expr.Number(-1000)),
        ("-1_000", expr.Number(-1000)),
        ("-16#6664#", expr.Number(-26212)),
        ("-16#66_64#", expr.Number(-26212)),
    ],
)
def test_expression_numeric_literal(string: str, expected: expr.Expr) -> None:
    actual = grammar.numeric_literal().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected", [("X", expr.Variable("X")), ("X::Y", expr.Variable("X::Y"))]
)
def test_variable(string: str, expected: decl.Declaration) -> None:
    actual = grammar.variable().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X'First", expr.First(expr.Variable("X"))),
        ("X'Last", expr.Last(expr.Variable("X"))),
        ("X'Length", expr.Length(expr.Variable("X"))),
        ("X'Head", expr.Head(expr.Variable("X"))),
        ("X'Opaque", expr.Opaque(expr.Variable("X"))),
        ("X'Present", expr.Present(expr.Variable("X"))),
        ("X'Valid", expr.Valid(expr.Variable("X"))),
        ("X'Valid_Checksum", expr.ValidChecksum(expr.Variable("X"))),
        ("X where X = 42", expr.Binding(expr.Variable("X"), {ID("X"): expr.Number(42)})),
        ("X'Head.Y", expr.Selected(expr.Head(expr.Variable("X")), "Y")),
    ],
)
def test_expression_suffix(string: str, expected: expr.Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A - B * 2**3 - 1",
            expr.Sub(
                expr.Sub(
                    expr.Variable("A"),
                    expr.Mul(expr.Variable("B"), expr.Pow(expr.Number(2), expr.Number(3))),
                ),
                expr.Number(1),
            ),
        ),
        (
            "(A - B) * 2**3 - 1",
            expr.Sub(
                expr.Mul(
                    expr.Sub(expr.Variable("A"), expr.Variable("B")),
                    expr.Pow(expr.Number(2), expr.Number(3)),
                ),
                expr.Number(1),
            ),
        ),
        (
            "A - B * 2**(3 - 1)",
            expr.Sub(
                expr.Variable("A"),
                expr.Mul(
                    expr.Variable("B"),
                    expr.Pow(expr.Number(2), expr.Sub(expr.Number(3), expr.Number(1))),
                ),
            ),
        ),
        (
            "A - (B * 2)**3 - 1",
            expr.Sub(
                expr.Sub(
                    expr.Variable("A"),
                    expr.Pow(expr.Mul(expr.Variable("B"), expr.Number(2)), expr.Number(3)),
                ),
                expr.Number(1),
            ),
        ),
        (
            "A - (B * 2**3 - 1)",
            expr.Sub(
                expr.Variable("A"),
                expr.Sub(
                    expr.Mul(expr.Variable("B"), expr.Pow(expr.Number(2), expr.Number(3))),
                    expr.Number(1),
                ),
            ),
        ),
        (
            "A + B * (-8)",
            expr.Add(expr.Variable("A"), expr.Mul(expr.Variable("B"), expr.Number(-8))),
        ),
    ],
)
def test_expression_mathematical(string: str, expected: expr.Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X = Y", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
        ("X /= Y", expr.NotEqual(expr.Variable("X"), expr.Variable("Y"))),
        ("42 < X", expr.Less(expr.Number(42), expr.Variable("X"))),
        ("42 <= X", expr.LessEqual(expr.Number(42), expr.Variable("X"))),
        ("X > 42", expr.Greater(expr.Variable("X"), expr.Number(42))),
        ("X >= 42", expr.GreaterEqual(expr.Variable("X"), expr.Number(42))),
        ("X in Y", expr.In(expr.Variable("X"), expr.Variable("Y"))),
        ("X not in Y", expr.NotIn(expr.Variable("X"), expr.Variable("Y"))),
        ("((X = 42))", expr.Equal(expr.Variable("X"), expr.Number(42))),
    ],
)
def test_expression_relation(string: str, expected: expr.Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X or Y", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
        ("((X or Y))", expr.Or(expr.Variable("X"), expr.Variable("Y"))),
    ],
)
def test_expression_boolean(string: str, expected: expr.Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X + Y", expr.Add(expr.Variable("X"), expr.Variable("Y"))),
        ("X + Y (Z)", expr.Add(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_mathematical_expression(string: str, expected: expr.Expr) -> None:
    actual = grammar.mathematical_expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        ("42 > X", 'unexpected expression type "Greater".*'),
        ("X and Y", 'unexpected expression type "And".*'),
    ],
)
def test_mathematical_expression_error(string: str, error: expr.Expr) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.mathematical_expression().parseString(string, parseAll=True)


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X and Y", expr.And(expr.Variable("X"), expr.Variable("Y"))),
        ("X and Y (Z)", expr.And(expr.Variable("X"), expr.Call("Y", [expr.Variable("Z")]))),
    ],
)
def test_boolean_expression(string: str, expected: expr.Expr) -> None:
    actual = grammar.boolean_expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        ("42", 'unexpected expression type "Number".*'),
        ("X", 'unexpected expression type "Variable".*'),
    ],
)
def test_boolean_expression_error(string: str, error: expr.Expr) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.boolean_expression().parseString(string, parseAll=True)


@pytest.mark.parametrize(
    "string,expected",
    [
        ("42", expr.Number(42)),
        ('"Foo Bar"', expr.String("Foo Bar")),
        ("[1]", expr.Aggregate(expr.Number(1))),
        ("[1, 2]", expr.Aggregate(expr.Number(1), expr.Number(2))),
        (
            '[137] & "PNG" & [13, 10, 26, 10]',
            expr.Aggregate(
                expr.Number(137),
                expr.Number(80),
                expr.Number(78),
                expr.Number(71),
                expr.Number(13),
                expr.Number(10),
                expr.Number(26),
                expr.Number(10),
            ),
        ),
        (
            "for all X in Y => X = Z",
            expr.ForAllIn(
                "X", expr.Variable("Y"), expr.Equal(expr.Variable("X"), expr.Variable("Z"))
            ),
        ),
        (
            "for some X in Y => X = Z",
            expr.ForSomeIn(
                "X", expr.Variable("Y"), expr.Equal(expr.Variable("X"), expr.Variable("Z"))
            ),
        ),
        (
            "[for X in Y => X.A]",
            expr.Comprehension(
                "X",
                expr.Variable("Y"),
                expr.Selected(expr.Variable("X"), "A"),
                expr.TRUE,
            ),
        ),
        (
            "[for X in Y => X.A when X.B = Z]",
            expr.Comprehension(
                "X",
                expr.Variable("Y"),
                expr.Selected(expr.Variable("X"), "A"),
                expr.Equal(expr.Selected(expr.Variable("X"), "B"), expr.Variable("Z")),
            ),
        ),
        (
            'X (A, "S", 42)',
            expr.Call("X", [expr.Variable("A"), expr.String("S"), expr.Number(42)]),
        ),
        ("X::Y (A)", expr.Conversion("X::Y", expr.Variable("A"))),
        ("X'(Y => Z)", expr.MessageAggregate("X", {ID("Y"): expr.Variable("Z")})),
        (
            "X'(Y => Z, A => B)",
            expr.MessageAggregate("X", {ID("Y"): expr.Variable("Z"), ID("A"): expr.Variable("B")}),
        ),
        ("X'(null message)", expr.MessageAggregate("X", {})),
        ("X", expr.Variable("X")),
    ],
)
def test_expression_base(string: str, expected: expr.Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X'Valid = True", expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("True"))),
        ("X::Y /= Z", expr.NotEqual(expr.Variable("X::Y"), expr.Variable("Z"))),
        ("X.Y /= Z", expr.NotEqual(expr.Selected(expr.Variable("X"), "Y"), expr.Variable("Z"))),
        (
            "X = Y and Y /= Z",
            expr.And(
                expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                expr.NotEqual(expr.Variable("Y"), expr.Variable("Z")),
            ),
        ),
        (
            "X'Valid and Y'Valid",
            expr.And(expr.Valid(expr.Variable("X")), expr.Valid(expr.Variable("Y"))),
        ),
        (
            "X'Valid = True and (Y'Valid = False or Z'Valid = False)",
            expr.And(
                expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("True")),
                expr.Or(
                    expr.Equal(expr.Valid(expr.Variable("Y")), expr.Variable("False")),
                    expr.Equal(expr.Valid(expr.Variable("Z")), expr.Variable("False")),
                ),
            ),
        ),
        (
            "X'Valid = False or X.A /= A or X.B /= B or (X.C = 0 and X.D not in X.E)",
            expr.Or(
                expr.Or(
                    expr.Or(
                        expr.Equal(expr.Valid(expr.Variable("X")), expr.Variable("False")),
                        expr.NotEqual(expr.Selected(expr.Variable("X"), "A"), expr.Variable("A")),
                    ),
                    expr.NotEqual(expr.Selected(expr.Variable("X"), "B"), expr.Variable("B")),
                ),
                expr.And(
                    expr.Equal(expr.Selected(expr.Variable("X"), "C"), expr.Number(0)),
                    expr.NotIn(
                        expr.Selected(expr.Variable("X"), "D"),
                        expr.Selected(expr.Variable("X"), "E"),
                    ),
                ),
            ),
        ),
        (
            "for some A in X.B => (A.T = P.E and (G::E not in P::S (A.D).V))",
            expr.ForSomeIn(
                "A",
                expr.Selected(expr.Variable("X"), "B"),
                expr.And(
                    expr.Equal(
                        expr.Selected(expr.Variable("A"), "T"),
                        expr.Selected(expr.Variable("P"), "E"),
                    ),
                    expr.NotIn(
                        expr.Variable("G::E"),
                        expr.Selected(
                            expr.Conversion("P::S", expr.Selected(expr.Variable("A"), "D")),
                            "V",
                        ),
                    ),
                ),
            ),
        ),
        ("X::Y (Z) = 42", expr.Equal(expr.Conversion("X::Y", expr.Variable("Z")), expr.Number(42))),
        ("X (Y).Z", expr.Selected(expr.Call("X", [expr.Variable("Y")]), "Z")),
        ("X (Y).Z'Length", expr.Length(expr.Selected(expr.Call("X", [expr.Variable("Y")]), "Z"))),
        (
            "G::E not in P::S (E.D).V",
            expr.NotIn(
                expr.Variable("G::E"),
                expr.Selected(expr.Conversion("P::S", expr.Selected(expr.Variable("E"), "D")), "V"),
            ),
        ),
        (
            "[for E in L => E.B when E.T = A]'Head",
            expr.Head(
                expr.Comprehension(
                    "E",
                    expr.Variable("L"),
                    expr.Selected(expr.Variable("E"), "B"),
                    expr.Equal(expr.Selected(expr.Variable("E"), "T"), expr.Variable("A")),
                )
            ),
        ),
        ("A'Head.D", expr.Selected(expr.Head(expr.Variable("A")), "D")),
        (
            "[for E in L => E.B when E.T = A]'Head.D",
            expr.Selected(
                expr.Head(
                    expr.Comprehension(
                        "E",
                        expr.Variable("L"),
                        expr.Selected(expr.Variable("E"), "B"),
                        expr.Equal(expr.Selected(expr.Variable("E"), "T"), expr.Variable("A")),
                    )
                ),
                "D",
            ),
        ),
        (
            "(for some S in P::X ([for E in C.A => E when E.T = P.L]'Head.D).H => S.G = G) = False",
            expr.Equal(
                expr.ForSomeIn(
                    "S",
                    expr.Selected(
                        expr.Conversion(
                            "P::X",
                            expr.Selected(
                                expr.Head(
                                    expr.Comprehension(
                                        "E",
                                        expr.Selected(expr.Variable("C"), "A"),
                                        expr.Variable("E"),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("E"), "T"),
                                            expr.Selected(expr.Variable("P"), "L"),
                                        ),
                                    )
                                ),
                                "D",
                            ),
                        ),
                        "H",
                    ),
                    expr.Equal(expr.Selected(expr.Variable("S"), "G"), expr.Variable("G")),
                ),
                expr.Variable("False"),
            ),
        ),
        (
            "M1'(D => B1) where B1 = M2'(D => B2)",
            expr.Binding(
                expr.MessageAggregate("M1", {ID("D"): expr.Variable("B1")}),
                {ID("B1"): expr.MessageAggregate("M2", {ID("D"): expr.Variable("B2")})},
            ),
        ),
        (
            "M1'(D1 => B1, D2 => B2) where B1 = M2'(D => B2), B2 = M2'(D => B3)",
            expr.Binding(
                expr.MessageAggregate(
                    "M1", {ID("D1"): expr.Variable("B1"), ID("D2"): expr.Variable("B2")}
                ),
                {
                    ID("B1"): expr.MessageAggregate("M2", {ID("D"): expr.Variable("B2")}),
                    ID("B2"): expr.MessageAggregate("M2", {ID("D"): expr.Variable("B3")}),
                },
            ),
        ),
        (
            "M1'(D => B1) where B1 = M2'(D => B2) where B2 = M3'(D => B3)",
            expr.Binding(
                expr.MessageAggregate("M1", {ID("D"): expr.Variable("B1")}),
                {
                    ID("B1"): expr.Binding(
                        expr.MessageAggregate("M2", {ID("D"): expr.Variable("B2")}),
                        {ID("B2"): expr.MessageAggregate("M3", {ID("D"): expr.Variable("B3")})},
                    )
                },
            ),
        ),
    ],
)
def test_expression_complex(string: str, expected: expr.Expr) -> None:
    actual = grammar.expression().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


def test_private_type_declaration() -> None:
    string = "type X is private"
    expected = decl.TypeDeclaration(model.Private("X", location=Location((1, 1), None, (1, 17))))
    actual = grammar.private_type_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("X : Channel with Readable", decl.ChannelDeclaration("X", readable=True)),
        ("X : Channel with Writable", decl.ChannelDeclaration("X", writable=True)),
        (
            "X : Channel with Readable, Writable",
            decl.ChannelDeclaration("X", readable=True, writable=True),
        ),
    ],
)
def test_channel_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.channel_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("with function X return Y", decl.FunctionDeclaration("X", [], "Y")),
        (
            "with function X (A : B; C : D) return Y",
            decl.FunctionDeclaration("X", [decl.Argument("A", "B"), decl.Argument("C", "D")], "Y"),
        ),
    ],
)
def test_formal_function_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.formal_function_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("A : B", decl.VariableDeclaration("A", "B")),
        ("A : B := C", decl.VariableDeclaration("A", "B", expr.Variable("C"))),
        ("A : B := 1", decl.VariableDeclaration("A", "B", expr.Number(1))),
    ],
)
def test_variable_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.variable_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            "A : B renames C.D",
            decl.RenamingDeclaration("A", "B", expr.Selected(expr.Variable("C"), "D")),
        ),
    ],
)
def test_renaming_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.renaming_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [("A := B", stmt.Assignment("A", expr.Variable("B")))],
)
def test_assignment_statement(string: str, expected: stmt.Statement) -> None:
    actual = grammar.assignment_statement().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        ("A'Append (B)", stmt.Append("A", expr.Variable("B"))),
        ("A'Extend (B)", stmt.Extend("A", expr.Variable("B"))),
        ("A'Read (B)", stmt.Read("A", expr.Variable("B"))),
        ("A'Write (B)", stmt.Write("A", expr.Variable("B"))),
        ("C'Reset", stmt.Reset("C")),
    ],
)
def test_attribute_statement(string: str, expected: stmt.Statement) -> None:
    actual = grammar.attribute_statement().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            """
               state A is
               begin
               transition
                  then B
               end A
         """,
            model.State("A", transitions=[model.Transition("B")]),
        ),
        (
            """
               state A is
               begin
               transition
                  then B
                     if X = Y
                  then C
               end A
         """,
            model.State(
                "A",
                transitions=[
                    model.Transition("B", expr.Equal(expr.Variable("X"), expr.Variable("Y"))),
                    model.Transition("C"),
                ],
            ),
        ),
        (
            """
               state A is
               begin
               transition
                  then B
                     with Desc => "rfc2549.txt+12:3-45:6"
                     if X = Y
                  then C
                     with Desc => "rfc2549.txt+123:45-678:9"
               end A
         """,
            model.State(
                "A",
                transitions=[
                    model.Transition(
                        "B",
                        expr.Equal(expr.Variable("X"), expr.Variable("Y")),
                        description="rfc2549.txt+12:3-45:6",
                    ),
                    model.Transition("C", description="rfc2549.txt+123:45-678:9"),
                ],
            ),
        ),
        (
            """
               state A is
                  Z : Boolean := Y;
               begin
               transition
                  then B
               end A
         """,
            model.State(
                "A",
                transitions=[model.Transition("B")],
                declarations=[decl.VariableDeclaration("Z", "Boolean", expr.Variable("Y"))],
                actions=[],
            ),
        ),
    ],
)
def test_state(string: str, expected: decl.Declaration) -> None:
    actual = grammar.state().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        (
            """
               state A is
               begin
               transition
                  then B
               end C
         """,
            "inconsistent state identifier: A /= C.*",
        )
    ],
)
def test_state_error(string: str, error: str) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.state().parseString(string, parseAll=True)


@pytest.mark.parametrize(
    "string,expected",
    [
        (
            """
               generic
                  X : Channel with Readable, Writable;
                  type T is private;
                  with function F return T;
               session Session with
                  Initial => A,
                  Final => B
               is
                  Y : Boolean := 0;
               begin
                  state A is
                     Z : Boolean := Y;
                  begin
                     Z := 1;
                  transition
                     then B
                        if Z = 1
                     then A
                  end A;

                  state B is null state;
               end Session;
         """,
            ast.SessionSpec(
                ID("Session"),
                ID("A"),
                ID("B"),
                [
                    model.State(
                        "A",
                        declarations=[decl.VariableDeclaration("Z", "Boolean", expr.Variable("Y"))],
                        actions=[stmt.Assignment("Z", expr.Number(1))],
                        transitions=[
                            model.Transition(
                                "B", condition=expr.Equal(expr.Variable("Z"), expr.Number(1))
                            ),
                            model.Transition("A"),
                        ],
                    ),
                    model.State("B"),
                ],
                [decl.VariableDeclaration("Y", "Boolean", expr.Number(0))],
                [
                    decl.ChannelDeclaration("X", readable=True, writable=True),
                    decl.TypeDeclaration(model.Private("T")),
                    decl.FunctionDeclaration("F", [], "T"),
                ],
                Location((2, 16), None, (23, 27)),
            ),
        ),
    ],
)
def test_session_declaration(string: str, expected: decl.Declaration) -> None:
    actual = grammar.session_declaration().parseString(string, parseAll=True)[0]
    assert actual == expected
    assert actual.location


@pytest.mark.parametrize(
    "string,error",
    [
        (
            """
               generic
               session X with
                  Initial => A,
                  Final => A
               is
               begin
                  state A is null state;
               end Y;
         """,
            "inconsistent session identifier: X /= Y.*",
        )
    ],
)
def test_session_declaration_error(string: str, error: str) -> None:
    with pytest.raises(ParseFatalException, match=rf"^{error}$"):
        grammar.session_declaration().parseString(string, parseAll=True)


def test_session() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is

               generic
               session Session with
                  Initial => A,
                  Final => C
               is
               begin
                  state A is
                  begin
                  transition
                     then B
                  end A;

                  state B is
                  begin
                  transition
                     then C
                  end B;

                  state C is null state;
               end Session;

            end Test;
        """
    )
    p.create_model()


def test_unexpected_exception(monkeypatch: Any) -> None:
    with pytest.raises(
        ParseFatalException,
        match=r"ZeroDivisionError",
    ):
        monkeypatch.setattr(
            grammar,
            "parse_mathematical_operator",
            grammar.fatalexceptions(lambda x, y, z: [1, 1 / 0, 1]),
        )
        grammar.expression().parseString("1 + 1")


def test_expression_aggregate_no_number() -> None:
    with pytest.raises(ParseFatalException, match=r"^Expected Number"):
        grammar.expression().parseString("[1, Foo]")


def test_unexpected_suffix() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected suffix .*$"):
        grammar.parse_suffix(
            "",
            0,
            [[expr.Variable(ID("X")), ("X", None)]],
        )


def test_unexpected_relation_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected operator .*$"):
        grammar.parse_relational_operator(
            "",
            0,
            [
                [
                    expr.Number(1, location=Location((1, 1))),
                    "<>",
                    expr.Number(1, location=Location((1, 1))),
                ]
            ],
        )


def test_unexpected_boolean_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected operator .*$"):
        grammar.parse_boolean_operator(
            "",
            0,
            [
                [
                    expr.Number(1, location=Location((1, 8))),
                    "xor",
                    expr.Number(1, location=Location((2, 25))),
                ]
            ],
        )


def test_unexpected_mathematical_operator() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected operator .*$"):
        grammar.parse_mathematical_operator(
            "",
            0,
            [
                [
                    expr.Number(1, location=Location((1, 1))),
                    "//",
                    expr.Number(1, location=Location((1, 8))),
                ]
            ],
        )


def test_unexpected_quantified_expression(monkeypatch: Any) -> None:
    monkeypatch.setattr(grammar, "evaluate_located_expression", lambda s, t: (t, Location((1, 1))))
    with pytest.raises(ParseFatalException, match=r"^unexpected quantified expression"):
        grammar.parse_quantified_expression(
            "",
            0,
            [
                [
                    0,
                    "any",
                    ID("X"),
                    expr.Variable("Y"),
                    expr.Equal(expr.Variable("X"), expr.Variable("Z")),
                    24,
                ]
            ],
        )


def test_unexpected_type() -> None:
    with pytest.raises(ParseFatalException, match=r"^unexpected type"):
        grammar.parse_type("type T is X;", 0, [0, "type", "T", "is", "X", 8])


def test_unexpected_attribute(monkeypatch: Any) -> None:
    monkeypatch.setattr(grammar, "evaluate_located_expression", lambda s, t: (t, Location((1, 1))))
    with pytest.raises(ParseFatalException, match=r"^unexpected attribute"):
        grammar.parse_attribute("", 0, ["A", "Invalid", expr.Variable("B")])