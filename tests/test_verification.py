import unittest

from rflx.model import ModelError
from rflx.parser import Parser


# pylint: disable=too-many-public-methods
class TestVerification(unittest.TestCase):
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def assert_parse_exception_string(self, string: str, regex: str) -> None:
        with self.assertRaisesRegex(ModelError, regex):
            Parser().parse_string(string)

    @staticmethod
    def test_exclusive_valid() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then null
                                if F1 > 100,
                            then F2
                                if F1 <= 100;
                        F2 : Element;
                    end message;
            end Foo;
            """)

    @staticmethod
    def test_exclusive_enum_valid() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Kind is (Val1 => 1, Val2 => 2) with Size => 8;
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Kind
                            then null
                                if F1 = Val1,
                            then F2
                                if F1 = Val2;
                        F2 : Element;
                    end message;
            end Foo;
            """)

    def test_exclusive_conflict(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then null
                                if F1 > 50,
                            then F2
                                if F1 < 100;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^conflicting conditions for field "F1"')

    @staticmethod
    def test_exclusive_with_length_valid() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then null
                                if F1'Length = 32 and F1 < 50,
                            then F2
                                if F1'Length = 32 and F1 > 100;
                        F2 : Element;
                    end message;
            end Foo;
            """)

    def test_exclusive_with_length_invalid(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then null
                                if F1'Length = 32,
                            then F2
                                if F1'Length = 32;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^conflicting conditions for field "F1"')

    def test_no_valid_path(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then F2
                                if F1 <= 100,
                            then F3
                                if F1 > 100;
                        F2 : Element
                            then F3
                                if F1 > 100;
                        F3 : Element
                            then null
                                if F1 <= 100;
                    end message;
            end Foo;
            """,
            r'^unreachable field "Final"')

    def test_invalid_path_1(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element then null if 1 = 2;
                    end message;
            end Foo;
            """,
            r'^unreachable field "Final"')

    def test_invalid_path_2(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element then F2 if 1 = 2;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2"')

    def test_contradiction(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 0..2**32-1 with Size => 32;
                type Bar is
                    message
                        F1 : Element
                            then F2 if 1 = 2,
                            then F2 if F1 < 50,
                            then null if F1 > 100;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^contradicting condition 0 from field "F1" to "F2"')

    def test_invalid_type_condition_range_low(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 100 .. 1000 with Size => 32;
                type Bar is
                    message
                        F1 : Element then F2 if F1 < 100;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2"')

    def test_invalid_type_condition_range_high(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is range 5 .. 50000 with Size => 32;
                type Bar is
                    message
                        F1 : Element then F2 if F1 > 60000;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2"')

    def test_invalid_type_condition_modular_upper(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 if F1 > 1000;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2"')

    def test_invalid_type_condition_modular_lower(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 if F1 < 0;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^unreachable field "F2"')

    # ISSUE: Componolit/RecordFlux#87
    def disabled_test_invalid_type_condition_enum(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element1 is (E1, E2, E3) with Size => 8;
                type Element2 is (E4, E5, E6) with Size => 8;
                type Bar is
                    message
                        F1 : Element1 then F2 if F1 = E4;
                        F2 : Element2;
                    end message;
            end Foo;
            """,
            r'^invalid type of "E4" in condition 0 from field "F1" to "F2"')

    @staticmethod
    def test_tlv_valid_enum() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Tag is (T1, T2, T3) with Size => 8;
                type Length is range 0 .. 2**14 with Size => 16;
                type Bar is
                    message
                        L : Length;
                        T : Tag
                            then V
                                with Length => L
                                if T /= T2 and L <= 2**13;
                        V : Payload;
                    end message;
            end Foo;
            """)

    def test_invalid_fixed_size_field_with_length(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with Length => 300;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^fixed field "F2" with length attribute')

    @staticmethod
    def test_valid_first() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F1'First;
                        F2 : Element;
                    end message;
            end Foo;
            """)

    def test_invalid_first(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F1'First + 8;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^invalid start for field "F2" in First expression 0 from field "F1" to "F2"')

    def test_invalid_first_is_last(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F1'Last;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^invalid start for field "F2" in First expression 0 from field "F1" to "F2"')

    def test_invalid_first_forward_reference(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with First => F3'First;
                        F2 : Element;
                        F3 : Element;
                    end message;
            end Foo;
            """,
            r'^subsequent field "F3' "'" 'First" referenced in First expression 0 from field "F1"'
            r' to "F2"')

    @staticmethod
    def test_valid_length_reference() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with Length => F1;
                        F2 : Payload;
                    end message;
            end Foo;
            """)

    def test_invalid_length_forward_reference(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        null then F1 with Length => F2;
                        F1 : Payload;
                        F2 : Element;
                    end message;
            end Foo;
            """,
            r'^subsequent field "F2" referenced in Length expression 0 from field "Initial"'
            r' to "F1"')

    def test_invalid_positive_field_length(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element then F2 with Length => F1 - 300;
                        F2 : Payload;
                    end message;
            end Foo;
            """,
            r'^negative length for field "F2" on path F1 -> F2')

    @staticmethod
    def test_valid_length_from_message_last() -> None:
        parser = Parser()
        parser.parse_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element;
                        F2 : Element
                            then null
                                with Length => Message'Last - F1'Last;
                    end message;
            end Foo;
            """)

    def test_payload_no_length(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Bar is
                    message
                        F1 : Element;
                        F2 : Payload;
                    end message;
            end Foo;
            """,
            r'^unconstrained field "F2" without length attribute')

    def test_array_no_length(self) -> None:
        self.assert_parse_exception_string(
            """
            package Foo is
                type Element is mod 2**8;
                type Element_List is array of Element;
                type Bar is
                    message
                        F1 : Element;
                        F2 : Element_List;
                    end message;
            end Foo;
            """,
            r'^unconstrained field "F2" without length attribute')
