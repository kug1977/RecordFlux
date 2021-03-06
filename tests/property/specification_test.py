import math

from hypothesis import HealthCheck, given, settings, strategies as st

import rflx.expression as expr
from rflx.model import Model
from rflx.specification import Parser, grammar
from tests.property import strategies


@given(
    strategies.mathematical_expressions(
        st.one_of(
            strategies.numbers()
            | strategies.variables(strategies.identifiers())
            | strategies.attributes(strategies.identifiers())
        )
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_mathematical_expressions(expression: expr.Expr) -> None:
    parsed_expression = grammar.mathematical_expression().parseString(
        str(expression), parseAll=True
    )[0]
    assert parsed_expression == expression


@given(
    strategies.boolean_expressions(
        st.one_of(
            strategies.aggregates(strategies.numbers())
            | strategies.strings()
            | strategies.mathematical_expressions(
                st.one_of(
                    strategies.numbers()
                    | strategies.variables(strategies.identifiers())
                    | strategies.attributes(strategies.identifiers())
                )
            )
        )
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_boolean_expressions(expression: expr.Expr) -> None:
    parsed_expression = grammar.boolean_expression().parseString(str(expression), parseAll=True)[0]
    assert parsed_expression == expression


@given(
    st.one_of(
        strategies.mathematical_expressions(
            st.one_of(
                strategies.numbers()
                | strategies.variables(strategies.identifiers())
                | strategies.attributes(strategies.identifiers())
            )
        ),
        strategies.boolean_expressions(
            st.one_of(
                strategies.aggregates(strategies.numbers())
                | strategies.strings()
                | strategies.mathematical_expressions(
                    st.one_of(
                        strategies.numbers()
                        | strategies.variables(strategies.identifiers())
                        | strategies.attributes(strategies.identifiers())
                    )
                )
            )
        ),
        strategies.calls(
            st.one_of(
                strategies.numbers()
                | strategies.variables(strategies.identifiers())
                | strategies.attributes(strategies.identifiers())
            )
        ),
        strategies.quantified_expressions(
            st.one_of(
                strategies.numbers()
                | strategies.variables(strategies.identifiers())
                | strategies.attributes(strategies.identifiers())
            )
        ),
    )
)
@settings(deadline=None, suppress_health_check=[HealthCheck.filter_too_much, HealthCheck.too_slow])
def test_parsing_expressions(expression: expr.Expr) -> None:
    parsed_expression = grammar.expression().parseString(str(expression), parseAll=True)[0]
    assert parsed_expression == expression


@given(strategies.models())
@settings(
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
    max_examples=math.ceil(settings.default.max_examples / 10),
)
def test_parsing_model(model: Model) -> None:
    parser = Parser()
    parser.parse_string(
        f"""
        package Test is
            {model}
        end Test;
        """
    )
    parsed_model = parser.create_model()
    assert parsed_model.types == model.types
    assert parsed_model == model
