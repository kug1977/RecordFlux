from pathlib import Path
from typing import Sequence

import pytest

from rflx.error import RecordFluxError
from rflx.specification import parser
from tests.const import SPEC_DIR


def assert_error_files(filenames: Sequence[str], regex: str) -> None:
    assert " model: error: " in regex
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        for filename in filenames:
            p.parse(Path(filename))
        p.create_model()


def assert_error_string(string: str, regex: str) -> None:
    assert " model: error: " in regex
    p = parser.Parser()
    with pytest.raises(RecordFluxError, match=regex):
        p.parse_string(string)
        p.create_model()


def test_message_undefined_type() -> None:
    assert_error_string(
        """
            package Test is
               type PDU is
                  message
                     Foo : T;
                  end message;
            end Test;
        """,
        r'^<stdin>:5:22: model: error: missing type for field "Foo" in "Test::PDU"$',
    )


def test_illegal_redefinition() -> None:
    assert_error_string(
        """
            package Test is
               type Boolean is mod 2;
            end Test;
        """,
        r'^<stdin>:3:16: model: error: illegal redefinition of built-in type "Boolean"',
    )


def test_invalid_modular_type() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 2**128;
            end Test;
        """,
        r'^<stdin>:3:30: model: error: modulus of "T" exceeds limit \(2\*\*64\)',
    )


def test_invalid_enumeration_type_size() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo, Bar, Baz) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:16: model: error: size of "T" too small',
    )


def test_invalid_enumeration_type_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo => 0, Bar => 0) with Size => 1;
            end Test;
        """,
        r'<stdin>:3:44: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_multiple_duplicate_values() -> None:
    assert_error_string(
        """
            package Test is
               type T is (Foo => 0, Foo_1 => 1, Bar => 0, Bar_1 => 1) with Size => 8;
            end Test;
        """,
        r'<stdin>:3:56: model: error: duplicate enumeration value "0" in "T"\n'
        r"<stdin>:3:34: model: info: previous occurrence\n"
        r'<stdin>:3:68: model: error: duplicate enumeration value "1" in "T"\n'
        r"<stdin>:3:46: model: info: previous occurrence",
    )


def test_invalid_enumeration_type_identical_literals_location() -> None:
    assert_error_files(
        [f"{SPEC_DIR}/identical_literals.rflx"],
        f"{SPEC_DIR}/identical_literals.rflx:3:4: model: error: conflicting literals: Bar\n"
        f'{SPEC_DIR}/identical_literals.rflx:2:21: model: info: previous occurrence of "Bar"',
    )


def test_refinement_invalid_field() -> None:
    assert_error_string(
        """
            package Test is
               type T is mod 256;
               type PDU is
                  message
                     Foo : T;
                  end message;
               for PDU use (Bar => PDU);
            end Test;
        """,
        r'^<stdin>:8:29: model: error: invalid field "Bar" in refinement',
    )


def test_message_with_two_length_fields() -> None:
    p = parser.Parser()
    p.parse_string(
        """
           package Test is
              type Length is mod 2**8;
              type Packet is
                 message
                    Length_1 : Length;
                    Length_2 : Length
                       then Payload
                          with Length => 8 * (Length_1 + Length_2);
                    Payload : Opaque;
                 end message;
           end Test;
        """
    )
    p.create_model()