import z3

from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Case,
    Div,
    Equal,
    Expr,
    Greater,
    GreaterEqual,
    If,
    Less,
    LessEqual,
    Mod,
    Mul,
    Not,
    NotEqual,
    Number,
    Or,
    Pow,
    Sub,
    Variable,
    rflx_expr,
)
from tests.utils import assert_equal


def assert_z3_expr(expr: Expr) -> None:
    result = rflx_expr(expr.z3expr())
    print(repr(expr))
    print(repr(result))
    assert expr == result


def test_true() -> None:
    assert TRUE.z3expr() == z3.BoolVal(True)


def test_false() -> None:
    assert FALSE.z3expr() == z3.BoolVal(False)


def test_not() -> None:
    assert Not(TRUE).z3expr() == z3.Not(z3.BoolVal(True))
    assert Not(FALSE).z3expr() == z3.Not(z3.BoolVal(False))


def test_and() -> None:
    assert_equal(
        And(TRUE, FALSE, TRUE).z3expr(),
        z3.And(z3.BoolVal(True), z3.BoolVal(False), z3.BoolVal(True)),
    )
    assert_equal(
        And(TRUE, TRUE, TRUE).z3expr(),
        z3.And(z3.BoolVal(True), z3.BoolVal(True), z3.BoolVal(True)),
    )
    assert_equal(And(TRUE, TRUE).z3expr(), z3.And(z3.BoolVal(True), z3.BoolVal(True)))


def test_or() -> None:
    assert_equal(
        Or(TRUE, FALSE, TRUE).z3expr(),
        z3.Or(z3.BoolVal(True), z3.BoolVal(False), z3.BoolVal(True)),
    )
    assert_equal(
        Or(TRUE, TRUE, TRUE).z3expr(), z3.Or(z3.BoolVal(True), z3.BoolVal(True), z3.BoolVal(True)),
    )
    assert_equal(Or(TRUE, TRUE).z3expr(), z3.Or(z3.BoolVal(True), z3.BoolVal(True)))


def test_int() -> None:
    assert Number(42).z3expr() == z3.IntVal(42)


def test_add() -> None:
    assert Add(Number(42), Number(1)).z3expr() == z3.IntVal(42) + z3.IntVal(1)
    assert_equal(
        Add(Number(42), Number(1), Number(10)).z3expr(),
        z3.Sum(z3.IntVal(42), z3.IntVal(1), z3.IntVal(10)),
    )


def test_mul() -> None:
    assert Mul(Number(6), Number(4)).z3expr() == z3.IntVal(6) * z3.IntVal(4)
    assert_equal(
        Mul(Number(2), Number(4), Number(8)).z3expr(),
        z3.Product(z3.IntVal(2), z3.IntVal(4), z3.IntVal(8)),
    )


def test_sub() -> None:
    assert Sub(Number(6), Number(4)).z3expr() == z3.IntVal(6) - z3.IntVal(4)
    assert Sub(Number(12), Number(20)).z3expr() == z3.IntVal(12) - z3.IntVal(20)


def test_div() -> None:
    assert Div(Number(6), Number(3)).z3expr() == z3.IntVal(6) / z3.IntVal(3)


def test_pow() -> None:
    assert Pow(Number(6), Number(2)).z3expr() == z3.IntVal(6) ** z3.IntVal(2)


def test_mod() -> None:
    assert Mod(Number(1000), Number(5)).z3expr() == z3.IntVal(1000) % z3.IntVal(5)


def test_variable() -> None:
    assert Variable("RecordFlux").z3expr() == z3.Int("RecordFlux")
    assert z3.simplify(Sub(Variable("foo"), Variable("foo")).z3expr()) == z3.IntVal(0)


def test_less_than() -> None:
    assert Less(Number(1), Number(100)).z3expr() == (z3.IntVal(1) < z3.IntVal(100))


def test_less_equal() -> None:
    assert LessEqual(Number(1), Number(100)).z3expr() == (z3.IntVal(1) <= z3.IntVal(100))


def test_equal() -> None:
    assert Equal(Number(100), Number(100)).z3expr() == (z3.IntVal(100) == z3.IntVal(100))


def test_greater_equal() -> None:
    assert GreaterEqual(Number(100), Number(1)).z3expr() == (z3.IntVal(100) >= z3.IntVal(1))


def test_greater() -> None:
    assert Greater(Number(100), Number(1)).z3expr() == (z3.IntVal(100) > z3.IntVal(1))


def test_not_equal() -> None:
    assert NotEqual(Number(100), Number(1)).z3expr() == (z3.IntVal(100) != z3.IntVal(1))


def test_if() -> None:
    assert_equal(
        If(
            [
                (Greater(Variable("a"), Number(5)), Number(1)),
                (Greater(Variable("b"), Number(100)), Number(10)),
            ],
            Number(100),
        ).z3expr(),
        z3.If(
            z3.Int("a") > z3.IntVal(5),
            z3.IntVal(1),
            z3.If(z3.Int("b") > z3.IntVal(100), z3.IntVal(10), z3.IntVal(100)),
        ),
    )


def test_case() -> None:
    assert_equal(
        Case(Variable("x"), [(Number(5), Number(1)), (Number(10), Number(2))]).z3expr(),
        z3.If(
            z3.Int("x") == z3.IntVal(5),
            z3.IntVal(1),
            z3.If(z3.Int("x") == z3.IntVal(10), z3.IntVal(2), z3.BoolVal(False)),
        ),
    )


def test_from_z3expr_bool_literal() -> None:
    assert_z3_expr(TRUE)
    assert_z3_expr(FALSE)


def test_from_z3expr_variable() -> None:
    assert_z3_expr(Variable("X"))
    assert_z3_expr(Variable("Foo_Bar"))


def test_from_z3expr_neg() -> None:
    assert_z3_expr(Not(TRUE))
    assert_z3_expr(Not(FALSE))
    assert_z3_expr(Not(Not(FALSE)))


def test_from_z3expr_conj() -> None:
    assert_z3_expr(And(TRUE, TRUE))
    assert_z3_expr(And(TRUE, FALSE))
    assert_z3_expr(And(And(TRUE, FALSE), FALSE))
    assert_z3_expr(And(Not(And(TRUE, FALSE)), Not(FALSE)))
    assert_z3_expr(And(TRUE, FALSE, FALSE, TRUE, Not(FALSE)))


def test_from_z3expr_disj() -> None:
    assert_z3_expr(Or(TRUE, TRUE))
    assert_z3_expr(Or(TRUE, FALSE))
    assert_z3_expr(Or(Or(TRUE, FALSE), FALSE))
    assert_z3_expr(Or(Not(And(TRUE, FALSE)), Not(FALSE)))
    assert_z3_expr(Or(TRUE, FALSE, FALSE, TRUE, Not(FALSE)))


def test_from_z3expr_number() -> None:
    assert_z3_expr(Number(0))
    assert_z3_expr(Number(42))
    assert_z3_expr(Number(-200))


def test_from_z3expr_add() -> None:
    assert_z3_expr(Add(Number(0), Number(20)))
    assert_z3_expr(Add(Number(40), Number(20), Number(30)))


def test_from_z3expr_mul() -> None:
    assert_z3_expr(Mul(Number(6), Number(4)))
    assert_z3_expr(Mul(Number(2), Number(4), Number(8)))


def test_from_z3expr_sub() -> None:
    assert_z3_expr(Sub(Number(6), Number(4)))
    assert_z3_expr(Sub(Number(12), Number(20)))


def test_from_z3expr_div() -> None:
    assert_z3_expr(Div(Number(6), Number(3)))


def test_from_z3expr_pow() -> None:
    assert_z3_expr(Pow(Number(6), Number(3)))


def test_from_z3expr_mod() -> None:
    assert_z3_expr(Mod(Number(1000), Number(5)))


def test_from_z3expr_less() -> None:
    assert_z3_expr(Less(Number(1), Number(100)))


def test_from_z3expr_less_equal() -> None:
    assert_z3_expr(LessEqual(Number(1), Number(100)))


def test_from_z3expr_equal() -> None:
    assert_z3_expr(Equal(Number(100), Number(100)))


def test_from_z3expr_greater_equal() -> None:
    assert_z3_expr(GreaterEqual(Number(100), Number(1)))


def test_from_z3expr_greater() -> None:
    assert_z3_expr(Greater(Number(100), Number(1)))


def test_from_z3expr_not_equal() -> None:
    assert_z3_expr(NotEqual(Number(100), Number(1)))
