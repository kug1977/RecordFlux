from typing import List

from pyparsing import Keyword, Token, infixNotation, opAssoc

from rflx.expression import FALSE, TRUE, Equal, Expr, NotEqual, Variable
from rflx.parser import Parser


class FSMParser:
    @classmethod
    def __parse_equation(cls, tokens: List[List[Expr]]) -> Expr:
        t = tokens[0]
        op = t[1]

        if op == "=":
            return Equal(t[0], t[2])
        if op == "/=":
            return NotEqual(t[0], t[2])
        raise NotImplementedError(f"operator {op} not implemented")

    @classmethod
    def expression(cls) -> Token:
        literal = Parser.boolean_literal()
        literal.setParseAction(lambda t: TRUE if t[0] == "True" else FALSE)

        identifier = Parser.qualified_identifier()
        identifier.setParseAction(lambda t: Variable("".join(t)))

        simple_expression = literal | identifier

        equation = infixNotation(
            simple_expression, [(Keyword("="), 2, opAssoc.LEFT), (Keyword("/="), 2, opAssoc.LEFT)]
        )
        equation.setParseAction(cls.__parse_equation)

        return equation

    @classmethod
    def condition(cls) -> Token:
        return cls.expression()