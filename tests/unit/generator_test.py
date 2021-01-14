# pylint: disable=too-many-lines

from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Mapping, Tuple

import pytest

from rflx import ada, declaration as decl, expression as expr, statement as stmt
from rflx.error import RecordFluxError
from rflx.generator import Generator, common, const
from rflx.identifier import ID
from rflx.model import BOOLEAN, BUILTIN_TYPES, Model, Session, State, Transition, Type
from tests.const import GENERATED_DIR
from tests.data import models
from tests.utils import assert_compilable_code, assert_equal, assert_provable_code


def units(generator: Generator) -> Mapping[str, str]:
    result = {}
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.name.startswith("rflx-p"):
            result[f"{unit.name}.ads"] = unit.ads
            result[f"{unit.name}.adb"] = unit.adb
    return result


def assert_specification(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        with open(f"{GENERATED_DIR}/{unit.name}.ads", "r") as f:
            assert unit.ads == f.read(), unit.name


def assert_body(generator: Generator) -> None:
    for unit in generator._units.values():  # pylint: disable=protected-access
        if unit.adb:
            with open(f"{GENERATED_DIR}/{unit.name}.adb", "r") as f:
                assert unit.adb == f.read(), unit.name


def generate(model: Model) -> Generator:
    generator = Generator(model, "RFLX", reproducible=True)
    return generator


def test_invalid_prefix() -> None:
    with pytest.raises(RecordFluxError, match=r"^id: error: empty part in identifier$"):
        Generator(Model(), "A..B")


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_unexpected_type() -> None:
    class TestType(Type):
        pass

    with pytest.raises(AssertionError, match='unexpected type "TestType"'):
        Generator(Model([TestType("P::T")]))


def test_library_files(tmp_path: Path) -> None:
    generator = Generator(Model(), "RFLX", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in [f"rflx-{f}" for f in const.LIBRARY_FILES]:
        with open(tmp_path / filename) as library_file:
            with open(GENERATED_DIR / filename) as expected_file:
                assert library_file.read() == expected_file.read(), filename


def test_library_files_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), "", reproducible=True)
    generator.write_library_files(tmp_path)
    for filename in const.LIBRARY_FILES:
        assert (tmp_path / filename).exists()


def test_top_level_package(tmp_path: Path) -> None:
    generator = Generator(Model(), "RFLX", reproducible=True)
    generator.write_top_level_package(tmp_path)

    created_files = list(tmp_path.glob("*"))
    assert created_files == [tmp_path / Path("rflx.ads")]

    for created_file in created_files:
        with open(created_file) as library_file:
            with open(GENERATED_DIR / created_file.name) as expected_file:
                assert library_file.read() == expected_file.read(), created_file.name


def test_top_level_package_no_prefix(tmp_path: Path) -> None:
    generator = Generator(Model(), "", reproducible=True)
    generator.write_top_level_package(tmp_path)
    assert list(tmp_path.glob("*")) == []


@pytest.mark.parametrize(
    "left,right",
    [
        (expr.Variable("Value"), expr.Aggregate(expr.Number(1), expr.Number(2))),
        (expr.Aggregate(expr.Number(1), expr.Number(2)), expr.Variable("Value")),
    ],
)
@pytest.mark.parametrize("relation", [expr.Equal, expr.NotEqual])
def test_substitution_relation_aggregate(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation], left: expr.Expr, right: expr.Expr
) -> None:
    equal_call = expr.Call(
        "Equal",
        [
            expr.Variable("Ctx"),
            expr.Variable("F_Value"),
            expr.Aggregate(
                expr.Val(expr.Variable(expr.ID("Types") * "Byte"), expr.Number(1)),
                expr.Val(expr.Variable(expr.ID("Types") * "Byte"), expr.Number(2)),
            ),
        ],
    )

    assert_equal(
        relation(left, right).substituted(common.substitution(models.TLV_MESSAGE)),
        equal_call if relation == expr.Equal else expr.Not(equal_call),
    )


@pytest.mark.parametrize(
    "expressions,expected",
    [
        (
            (expr.Variable("Length"), expr.Number(1)),
            (expr.Call("Get_Length", [expr.Variable("Ctx")]), expr.Number(1)),
        ),
        (
            (expr.Number(1), expr.Variable("Length")),
            (expr.Number(1), expr.Call("Get_Length", [expr.Variable("Ctx")])),
        ),
        ((expr.Number(1), expr.Variable("Unknown")), (expr.Number(1), expr.Variable("Unknown"))),
    ],
)
@pytest.mark.parametrize(
    "relation",
    [expr.Less, expr.LessEqual, expr.Equal, expr.GreaterEqual, expr.Greater, expr.NotEqual],
)
def test_substitution_relation_scalar(
    relation: Callable[[expr.Expr, expr.Expr], expr.Relation],
    expressions: Tuple[expr.Expr, expr.Expr],
    expected: Tuple[expr.Expr, expr.Expr],
) -> None:
    assert_equal(
        relation(*expressions).substituted(common.substitution(models.TLV_MESSAGE, public=True)),
        relation(*expected),
    )


def test_prefixed_type_identifier() -> None:
    assert common.prefixed_type_identifier(ada.ID("Modular"), "P") == ada.ID("P.Modular")
    for t in BUILTIN_TYPES:
        assert common.prefixed_type_identifier(ada.ID(t), "P") == t


def test_base_type_name() -> None:
    assert common.base_type_name(models.MODULAR_INTEGER) == ada.ID("Modular")
    assert common.base_type_name(models.RANGE_INTEGER) == ada.ID("Range_Integer_Base")


def test_full_base_type_name() -> None:
    assert common.full_base_type_name(models.MODULAR_INTEGER) == ada.ID("P.Modular")
    assert common.full_base_type_name(models.RANGE_INTEGER) == ada.ID("P.Range_Integer_Base")


@pytest.mark.parametrize(
    "model",
    [
        models.NULL_MODEL,
        models.TLV_MODEL,
        models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
        models.ETHERNET_MODEL,
        models.ENUMERATION_MODEL,
        models.ARRAYS_MODEL,
        models.EXPRESSION_MODEL,
        models.DERIVATION_MODEL,
    ],
)
def test_specification(model: Model) -> None:
    generator = generate(model)
    assert_specification(generator)


@pytest.mark.parametrize(
    "model",
    [
        models.NULL_MODEL,
        models.TLV_MODEL,
        models.NULL_MESSAGE_IN_TLV_MESSAGE_MODEL,
        models.ETHERNET_MODEL,
        models.ENUMERATION_MODEL,
        models.ARRAYS_MODEL,
        models.EXPRESSION_MODEL,
        models.DERIVATION_MODEL,
    ],
)
def test_body(model: Model) -> None:
    generator = generate(model)
    assert_body(generator)


@dataclass
class TestCase:
    model: Model
    expected: Mapping[str, str]


TEST_CASES = {
    "empty_model": TestCase(
        Model([], []),
        {},
    ),
    "scalar_type": TestCase(
        Model(
            [models.RANGE_INTEGER],
            [],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

   type Range_Integer_Base is mod 2**8 with
     Annotate =>
       (GNATprove, No_Wrap_Around);

   type Range_Integer is range 1 .. 100 with
     Size =>
       8;

   pragma Warnings (Off, "precondition is * false");

   function Unreachable_P_Range_Integer return RFLX.P.Range_Integer is
     (RFLX.P.Range_Integer\'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is * false");

   function Valid (Val : RFLX.P.Range_Integer_Base) return Boolean is
     (Val >= 1
      and Val <= 100);

   function To_Base (Val : RFLX.P.Range_Integer) return RFLX.P.Range_Integer_Base is
     (RFLX.P.Range_Integer_Base (Val));

   function To_Actual (Val : RFLX.P.Range_Integer_Base) return RFLX.P.Range_Integer is
     (RFLX.P.Range_Integer (Val))
    with
     Pre =>
       Valid (Val);

end RFLX.P;
"""[
                1:
            ],
        },
    ),
    "session_states": TestCase(
        Model(
            [],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State("Start", transitions=[Transition(target="End")]),
                        State("End"),
                    ],
                    declarations=[],
                    parameters=[],
                    types=[],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Unreferenced (Types);

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) is
   begin
      State := S_End;
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      while State /= S_End loop
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_global_variable": TestCase(
        Model(
            [],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target="End",
                                    condition=expr.Variable("Global"),
                                ),
                                Transition(
                                    target="Start",
                                ),
                            ],
                            declarations=[],
                            actions=[
                                stmt.Assignment("Global", expr.TRUE),
                            ],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "Boolean")],
                    parameters=[],
                    types=[BOOLEAN],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Unreferenced (Types);

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   Global : Boolean;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) is
   begin
      Global := True;
      if Global then
         State := S_End;
      else
         State := S_Start;
      end if;
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      while State /= S_End loop
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_state_variable": TestCase(
        Model(
            [],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                    condition=expr.Variable("Local"),
                                ),
                                Transition(
                                    target=ID("Start"),
                                ),
                            ],
                            declarations=[decl.VariableDeclaration("Local", "Boolean")],
                            actions=[
                                stmt.Assignment("Local", expr.TRUE),
                            ],
                        ),
                        State("End"),
                    ],
                    declarations=[],
                    parameters=[],
                    types=[BOOLEAN],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Unreferenced (Types);

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) is
      Local : Boolean;
   begin
      Local := True;
      if Local then
         State := S_End;
      else
         State := S_Start;
      end if;
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      while State /= S_End loop
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_variable_initialization": TestCase(
        Model(
            [],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                    condition=expr.Equal(
                                        expr.Variable("Local"), expr.Variable("Global")
                                    ),
                                ),
                                Transition(
                                    target=ID("Start"),
                                ),
                            ],
                            declarations=[decl.VariableDeclaration("Local", "Boolean", expr.FALSE)],
                            actions=[
                                stmt.Assignment("Global", expr.TRUE),
                                stmt.Assignment(
                                    "Local", expr.Or(expr.Variable("Local"), expr.TRUE)
                                ),
                            ],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "Boolean", expr.FALSE)],
                    parameters=[],
                    types=[BOOLEAN],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   pragma Unreferenced (Types);

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   Global : Boolean := False;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) is
      Local : Boolean := False;
   begin
      Global := True;
      Local := Local
      or else True;
      if Local = Global then
         State := S_End;
      else
         State := S_Start;
      end if;
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      while State /= S_End loop
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_global_variable_message": TestCase(
        Model(
            models.TLV_MODEL.types,
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                    condition=expr.And(
                                        expr.Valid("Global"),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("Global"), "Tag"),
                                            expr.Variable("TLV::Msg_Data"),
                                        ),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("Global"), "Length"),
                                            expr.Number(1),
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("Start"),
                                ),
                            ],
                            declarations=[],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "TLV::Message")],
                    parameters=[],
                    types=models.TLV_MODEL.types,
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;
with RFLX.TLV.Message;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types, RFLX.TLV.Message);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Generic_Message;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package TLV_Message is new RFLX.TLV.Generic_Message (Types, others => <>);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   use type Types.Index;

   Global_Ctx : TLV_Message.Context;

   Global_Buffer : Types.Bytes_Ptr;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) with
     Pre =>
       TLV_Message.Has_Buffer (Global_Ctx),
     Post =>
       TLV_Message.Has_Buffer (Global_Ctx)
   is
   begin
      if
        TLV_Message.Structural_Valid_Message (Global_Ctx)
        and then TLV_Message.Get_Tag (Global_Ctx) = TLV.Msg_Data
        and then TLV_Message.Get_Length (Global_Ctx) = 1
      then
         State := S_End;
      else
         State := S_Start;
      end if;
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      if TLV_Message.Has_Buffer (Global_Ctx) then
         pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
         TLV_Message.Take_Buffer (Global_Ctx, Global_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
         Types.Free (Global_Buffer);
      end if;
      Global_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Message.Initialize (Global_Ctx, Global_Buffer);
      while State /= S_End loop
         pragma Loop_Invariant (TLV_Message.Has_Buffer (Global_Ctx));
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
      pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
      TLV_Message.Take_Buffer (Global_Ctx, Global_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
      Types.Free (Global_Buffer);
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_state_variable_message": TestCase(
        Model(
            models.TLV_MODEL.types,
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                    condition=expr.And(
                                        expr.Valid("Message"),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("Message"), "Tag"),
                                            expr.Variable("TLV::Msg_Data"),
                                        ),
                                        expr.Equal(
                                            expr.Selected(expr.Variable("Message"), "Length"),
                                            expr.Number(1),
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("Start"),
                                ),
                            ],
                            declarations=[decl.VariableDeclaration("Message", "TLV::Message")],
                        ),
                        State("End"),
                    ],
                    declarations=[],
                    parameters=[],
                    types=models.TLV_MODEL.types,
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;
with RFLX.TLV.Message;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types, RFLX.TLV.Message);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Generic_Message;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package TLV_Message is new RFLX.TLV.Generic_Message (Types, others => <>);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   use type Types.Index;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) is
      Message_Ctx : TLV_Message.Context;
      Message_Buffer : Types.Bytes_Ptr;
   begin
      Message_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Message.Initialize (Message_Ctx, Message_Buffer);
      if
        TLV_Message.Structural_Valid_Message (Message_Ctx)
        and then TLV_Message.Get_Tag (Message_Ctx) = TLV.Msg_Data
        and then TLV_Message.Get_Length (Message_Ctx) = 1
      then
         State := S_End;
      else
         State := S_Start;
      end if;
      pragma Warnings (Off, \"unused assignment to \"\"Message_Ctx\"\"\");
      TLV_Message.Take_Buffer (Message_Ctx, Message_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Message_Ctx\"\"\");
      Types.Free (Message_Buffer);
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      while State /= S_End loop
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_message_sequence_append": TestCase(
        Model(
            [*models.TLV_MODEL.types, models.TLV_MESSAGES],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),
                            ],
                            exception_transition=Transition(target=ID("End")),
                            declarations=[decl.VariableDeclaration("Local", "TLV::Messages")],
                            actions=[
                                stmt.Append(
                                    "Global",
                                    expr.MessageAggregate(
                                        "TLV::Message",
                                        {"Tag": expr.Variable("TLV::Msg_Error")},
                                    ),
                                ),
                                stmt.Append(
                                    "Local",
                                    expr.MessageAggregate(
                                        "TLV::Message",
                                        {
                                            "Tag": expr.Variable("TLV::Msg_Data"),
                                            "Length": expr.Number(1),
                                            "Value": expr.Aggregate(expr.Number(2)),
                                        },
                                    ),
                                ),
                            ],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "TLV::Messages")],
                    parameters=[],
                    types=[*models.TLV_MODEL.types, models.TLV_MESSAGES],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;
with RFLX.TLV.Message;
with RFLX.TLV.Messages;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types, RFLX.TLV.Message, RFLX.TLV.Messages);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Generic_Message;
with RFLX.RFLX_Message_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package TLV_Message is new RFLX.TLV.Generic_Message (Types, others => <>);
   with package TLV_Messages is new RFLX.RFLX_Message_Sequence (Types, TLV_Message.Context, TLV_Message.Initialize, TLV_Message.Take_Buffer, TLV_Message.Has_Buffer, TLV_Message.Message_Last, TLV_Message.Initialized, TLV_Message.Structural_Valid_Message);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   use type Types.Index, Types.Bit_Length;

   Global_Ctx : TLV_Messages.Context;

   Global_Buffer : Types.Bytes_Ptr;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) with
     Pre =>
       TLV_Messages.Has_Buffer (Global_Ctx),
     Post =>
       TLV_Messages.Has_Buffer (Global_Ctx)
   is
      Local_Ctx : TLV_Messages.Context;
      Local_Buffer : Types.Bytes_Ptr;
   begin
      Local_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Messages.Initialize (Local_Ctx, Local_Buffer);
      if
        not TLV_Messages.Has_Element (Global_Ctx)
        or TLV_Messages.Available_Space (Global_Ctx) < 2
      then
         State := S_End;
         pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
         TLV_Messages.Take_Buffer (Local_Ctx, Local_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
         Types.Free (Local_Buffer);
         return;
      end if;
      declare
         Global_Element_Ctx : TLV_Message.Context;
      begin
         TLV_Messages.Switch (Global_Ctx, Global_Element_Ctx);
         TLV_Message.Set_Tag (Global_Element_Ctx, TLV.Msg_Error);
         pragma Warnings (Off, \"unused assignment to \"\"Global_Element_Ctx\"\"\");
         TLV_Messages.Update (Global_Ctx, Global_Element_Ctx);
         pragma Warnings (On, \"unused assignment to \"\"Global_Element_Ctx\"\"\");
      end;
      if
        not TLV_Messages.Has_Element (Local_Ctx)
        or TLV_Messages.Available_Space (Local_Ctx) < 24
      then
         State := S_End;
         pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
         TLV_Messages.Take_Buffer (Local_Ctx, Local_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
         Types.Free (Local_Buffer);
         return;
      end if;
      declare
         Local_Element_Ctx : TLV_Message.Context;
      begin
         TLV_Messages.Switch (Local_Ctx, Local_Element_Ctx);
         TLV_Message.Set_Tag (Local_Element_Ctx, TLV.Msg_Data);
         TLV_Message.Set_Length (Local_Element_Ctx, 1);
         TLV_Message.Set_Value (Local_Element_Ctx, (Types.Index'First => Types.Byte'Val (2)));
         pragma Warnings (Off, \"unused assignment to \"\"Local_Element_Ctx\"\"\");
         TLV_Messages.Update (Local_Ctx, Local_Element_Ctx);
         pragma Warnings (On, \"unused assignment to \"\"Local_Element_Ctx\"\"\");
      end;
      State := S_End;
      pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
      TLV_Messages.Take_Buffer (Local_Ctx, Local_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
      Types.Free (Local_Buffer);
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      if TLV_Messages.Has_Buffer (Global_Ctx) then
         pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
         TLV_Messages.Take_Buffer (Global_Ctx, Global_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
         Types.Free (Global_Buffer);
      end if;
      Global_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Messages.Initialize (Global_Ctx, Global_Buffer);
      while State /= S_End loop
         pragma Loop_Invariant (TLV_Messages.Has_Buffer (Global_Ctx));
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
      pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
      TLV_Messages.Take_Buffer (Global_Ctx, Global_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
      Types.Free (Global_Buffer);
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_scalar_sequence_append": TestCase(
        Model(
            [models.TLV_TAG, models.TLV_TAGS],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),
                            ],
                            exception_transition=Transition(target=ID("End")),
                            declarations=[decl.VariableDeclaration("Local", "TLV::Tags")],
                            actions=[
                                stmt.Append(
                                    "Global",
                                    expr.Variable("TLV::Msg_Error"),
                                ),
                                stmt.Append(
                                    "Local",
                                    expr.Variable("TLV::Msg_Data"),
                                ),
                            ],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "TLV::Tags")],
                    parameters=[],
                    types=[models.TLV_TAG, models.TLV_TAGS],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;
with RFLX.TLV.Tags;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types, RFLX.TLV.Tags);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.RFLX_Scalar_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package TLV_Tags is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.TLV.Tag, RFLX.TLV.Tag_Base, RFLX.TLV.Valid, RFLX.TLV.To_Actual, RFLX.TLV.To_Base);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   use type Types.Index, Types.Bit_Length;

   Global_Ctx : TLV_Tags.Context;

   Global_Buffer : Types.Bytes_Ptr;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) with
     Pre =>
       TLV_Tags.Has_Buffer (Global_Ctx),
     Post =>
       TLV_Tags.Has_Buffer (Global_Ctx)
   is
      Local_Ctx : TLV_Tags.Context;
      Local_Buffer : Types.Bytes_Ptr;
   begin
      Local_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Tags.Initialize (Local_Ctx, Local_Buffer);
      if
        not TLV_Tags.Has_Element (Global_Ctx)
        or TLV_Tags.Available_Space (Global_Ctx) < TLV.Tag'Size
      then
         State := S_End;
         pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
         TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
         Types.Free (Local_Buffer);
         return;
      end if;
      TLV_Tags.Append_Element (Global_Ctx, TLV.Msg_Error);
      if
        not TLV_Tags.Has_Element (Local_Ctx)
        or TLV_Tags.Available_Space (Local_Ctx) < TLV.Tag'Size
      then
         State := S_End;
         pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
         TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
         Types.Free (Local_Buffer);
         return;
      end if;
      TLV_Tags.Append_Element (Local_Ctx, TLV.Msg_Data);
      State := S_End;
      pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
      TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
      Types.Free (Local_Buffer);
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      if TLV_Tags.Has_Buffer (Global_Ctx) then
         pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
         TLV_Tags.Take_Buffer (Global_Ctx, Global_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
         Types.Free (Global_Buffer);
      end if;
      Global_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Tags.Initialize (Global_Ctx, Global_Buffer);
      while State /= S_End loop
         pragma Loop_Invariant (TLV_Tags.Has_Buffer (Global_Ctx));
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
      pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
      TLV_Tags.Take_Buffer (Global_Ctx, Global_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
      Types.Free (Global_Buffer);
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_comprehension": TestCase(
        Model(
            [*models.TLV_MODEL.types, models.TLV_MESSAGES, models.TLV_TAGS],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                ),
                            ],
                            exception_transition=Transition(target=ID("End")),
                            declarations=[decl.VariableDeclaration("Local", "TLV::Tags")],
                            actions=[
                                stmt.Assignment(
                                    "Local",
                                    expr.Comprehension(
                                        "E",
                                        expr.Variable("Global"),
                                        expr.Selected(expr.Variable("E"), "Tag"),
                                        expr.Greater(
                                            expr.Selected(expr.Variable("E"), "Length"),
                                            expr.Number(0),
                                        ),
                                    ),
                                )
                            ],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "TLV::Messages")],
                    parameters=[],
                    types=[*models.TLV_MODEL.types, models.TLV_MESSAGES, models.TLV_TAGS],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;
with RFLX.TLV.Message;
with RFLX.TLV.Messages;
with RFLX.TLV.Tags;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types, RFLX.TLV.Message, RFLX.TLV.Messages, RFLX.TLV.Tags);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Generic_Message;
with RFLX.RFLX_Message_Sequence;
with RFLX.RFLX_Scalar_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package TLV_Message is new RFLX.TLV.Generic_Message (Types, others => <>);
   with package TLV_Messages is new RFLX.RFLX_Message_Sequence (Types, TLV_Message.Context, TLV_Message.Initialize, TLV_Message.Take_Buffer, TLV_Message.Has_Buffer, TLV_Message.Message_Last, TLV_Message.Initialized, TLV_Message.Structural_Valid_Message);
   with package TLV_Tags is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.TLV.Tag, RFLX.TLV.Tag_Base, RFLX.TLV.Valid, RFLX.TLV.To_Actual, RFLX.TLV.To_Base);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   use type Types.Index, Types.Bit_Length;

   Global_Ctx : TLV_Messages.Context;

   Global_Buffer : Types.Bytes_Ptr;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) with
     Pre =>
       TLV_Messages.Has_Buffer (Global_Ctx),
     Post =>
       TLV_Messages.Has_Buffer (Global_Ctx)
   is
      Local_Ctx : TLV_Tags.Context;
      Local_Buffer : Types.Bytes_Ptr;
      RFLX_Exception : Boolean := False;
   begin
      Local_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Tags.Initialize (Local_Ctx, Local_Buffer);
      while TLV_Messages.Has_Element (Global_Ctx) loop
         pragma Loop_Invariant (TLV_Messages.Has_Buffer (Global_Ctx));
         pragma Loop_Invariant (TLV_Tags.Has_Buffer (Local_Ctx));
         pragma Loop_Invariant (TLV_Tags.Valid (Local_Ctx));
         declare
            E_Ctx : TLV_Message.Context;
         begin
            TLV_Tags.Reset (Local_Ctx);
            TLV_Messages.Switch (Global_Ctx, E_Ctx);
            TLV_Message.Verify_Message (E_Ctx);
            if TLV_Message.Valid (E_Ctx, TLV_Message.F_Length) then
               if TLV_Message.Get_Length (E_Ctx) > 0 then
                  if
                    TLV_Tags.Has_Element (Local_Ctx)
                    and TLV_Tags.Available_Space (Local_Ctx) >= TLV.Tag'Size
                  then
                     TLV_Tags.Append_Element (Local_Ctx, TLV_Message.Get_Tag (E_Ctx));
                  else
                     RFLX_Exception := True;
                  end if;
               end if;
            else
               RFLX_Exception := True;
            end if;
            pragma Warnings (Off, \"unused assignment to \"\"E_Ctx\"\"\");
            TLV_Messages.Update (Global_Ctx, E_Ctx);
            pragma Warnings (On, \"unused assignment to \"\"E_Ctx\"\"\");
         end;
         if RFLX_Exception then
            State := S_End;
            pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
            TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
            pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
            Types.Free (Local_Buffer);
            return;
         end if;
      end loop;
      State := S_End;
      pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
      TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
      Types.Free (Local_Buffer);
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      if TLV_Messages.Has_Buffer (Global_Ctx) then
         pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
         TLV_Messages.Take_Buffer (Global_Ctx, Global_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
         Types.Free (Global_Buffer);
      end if;
      Global_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Messages.Initialize (Global_Ctx, Global_Buffer);
      while State /= S_End loop
         pragma Loop_Invariant (TLV_Messages.Has_Buffer (Global_Ctx));
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
      pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
      TLV_Messages.Take_Buffer (Global_Ctx, Global_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
      Types.Free (Global_Buffer);
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
    "session_sequence_head": TestCase(
        Model(
            [*models.TLV_MODEL.types, models.TLV_MESSAGES, models.TLV_TAGS],
            [
                Session(
                    identifier="P::S",
                    initial=ID("Start"),
                    final=ID("End"),
                    states=[
                        State(
                            "Start",
                            transitions=[
                                Transition(
                                    target=ID("End"),
                                    condition=expr.And(
                                        expr.Valid("Global"),
                                        expr.Greater(
                                            expr.Size(expr.Variable("Global")),
                                            expr.Number(0),
                                        ),
                                        expr.Equal(
                                            expr.Selected(
                                                expr.Head(expr.Variable("Global")), "Length"
                                            ),
                                            expr.Number(1),
                                        ),
                                    ),
                                ),
                                Transition(
                                    target=ID("End"),
                                ),
                            ],
                            # exception_transition=Transition(target=ID("End")),
                            declarations=[decl.VariableDeclaration("Local", "TLV::Tags")],
                            actions=[],
                        ),
                        State("End"),
                    ],
                    declarations=[decl.VariableDeclaration("Global", "TLV::Messages")],
                    parameters=[],
                    types=[*models.TLV_MODEL.types, models.TLV_MESSAGES, models.TLV_TAGS],
                )
            ],
        ),
        {
            "rflx-p.adb": "",
            "rflx-p.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package RFLX.P with
  SPARK_Mode
is

end RFLX.P;
"""[
                1:
            ],
            "rflx-p-s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.P.Generic_S;
with RFLX.RFLX_Types;
with RFLX.TLV.Message;
with RFLX.TLV.Messages;
with RFLX.TLV.Tags;

package RFLX.P.S is new RFLX.P.Generic_S (RFLX.RFLX_Types, RFLX.TLV.Message, RFLX.TLV.Messages, RFLX.TLV.Tags);
"""[
                1:
            ],
            "rflx-p-s.adb": "",
            "rflx-p-generic_s.ads": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.RFLX_Generic_Types;
with RFLX.TLV;
use RFLX.TLV;
with RFLX.TLV.Generic_Message;
with RFLX.RFLX_Message_Sequence;
with RFLX.RFLX_Scalar_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with package TLV_Message is new RFLX.TLV.Generic_Message (Types, others => <>);
   with package TLV_Messages is new RFLX.RFLX_Message_Sequence (Types, TLV_Message.Context, TLV_Message.Initialize, TLV_Message.Take_Buffer, TLV_Message.Has_Buffer, TLV_Message.Message_Last, TLV_Message.Initialized, TLV_Message.Structural_Valid_Message);
   with package TLV_Tags is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.TLV.Tag, RFLX.TLV.Tag_Base, RFLX.TLV.Valid, RFLX.TLV.To_Actual, RFLX.TLV.To_Base);
package RFLX.P.Generic_S with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   Executed : Boolean := False;

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run;

   pragma Warnings (On, "subprogram ""Run"" has no effect");

end RFLX.P.Generic_S;
"""[
                1:
            ],
            "rflx-p-generic_s.adb": """
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");

package body RFLX.P.Generic_S with
  SPARK_Mode
is

   use type Types.Index, Types.Bit_Length;

   Global_Ctx : TLV_Messages.Context;

   Global_Buffer : Types.Bytes_Ptr;

   type Session_State is (S_Start, S_End);

   procedure Start (State : out Session_State) with
     Pre =>
       TLV_Messages.Has_Buffer (Global_Ctx),
     Post =>
       TLV_Messages.Has_Buffer (Global_Ctx)
   is
      Local_Ctx : TLV_Tags.Context;
      Local_Buffer : Types.Bytes_Ptr;
      RFLX_Exception : Boolean := False;
   begin
      Local_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Tags.Initialize (Local_Ctx, Local_Buffer);
      while TLV_Messages.Has_Element (Global_Ctx) loop
         pragma Loop_Invariant (TLV_Messages.Has_Buffer (Global_Ctx));
         pragma Loop_Invariant (TLV_Tags.Has_Buffer (Local_Ctx));
         pragma Loop_Invariant (TLV_Tags.Valid (Local_Ctx));
         declare
            E_Ctx : TLV_Message.Context;
         begin
            TLV_Tags.Reset (Local_Ctx);
            TLV_Messages.Switch (Global_Ctx, E_Ctx);
            TLV_Message.Verify_Message (E_Ctx);
            if TLV_Message.Valid (E_Ctx, TLV_Message.F_Length) then
               if TLV_Message.Get_Length (E_Ctx) > 0 then
                  if
                    TLV_Tags.Has_Element (Local_Ctx)
                    and TLV_Tags.Available_Space (Local_Ctx) >= TLV.Tag'Size
                  then
                     TLV_Tags.Append_Element (Local_Ctx, TLV_Message.Get_Tag (E_Ctx));
                  else
                     RFLX_Exception := True;
                  end if;
               end if;
            else
               RFLX_Exception := True;
            end if;
            pragma Warnings (Off, \"unused assignment to \"\"E_Ctx\"\"\");
            TLV_Messages.Update (Global_Ctx, E_Ctx);
            pragma Warnings (On, \"unused assignment to \"\"E_Ctx\"\"\");
         end;
         if RFLX_Exception then
            State := S_End;
            pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
            TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
            pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
            Types.Free (Local_Buffer);
            return;
         end if;
      end loop;
      State := S_End;
      pragma Warnings (Off, \"unused assignment to \"\"Local_Ctx\"\"\");
      TLV_Tags.Take_Buffer (Local_Ctx, Local_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Local_Ctx\"\"\");
      Types.Free (Local_Buffer);
   end Start;

   procedure Run is
      State : Session_State := S_Start;
   begin
      if TLV_Messages.Has_Buffer (Global_Ctx) then
         pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
         TLV_Messages.Take_Buffer (Global_Ctx, Global_Buffer);
         pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
         Types.Free (Global_Buffer);
      end if;
      Global_Buffer := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      TLV_Messages.Initialize (Global_Ctx, Global_Buffer);
      while State /= S_End loop
         pragma Loop_Invariant (TLV_Messages.Has_Buffer (Global_Ctx));
         case State is
            when S_Start =>
               Start (State);
            when S_End =>
               Executed := True;
         end case;
      end loop;
      pragma Warnings (Off, \"unused assignment to \"\"Global_Ctx\"\"\");
      TLV_Messages.Take_Buffer (Global_Ctx, Global_Buffer);
      pragma Warnings (On, \"unused assignment to \"\"Global_Ctx\"\"\");
      Types.Free (Global_Buffer);
   end Run;

end RFLX.P.Generic_S;
"""[
                1:
            ],
        },
    ),
}


@pytest.mark.parametrize("test_case", TEST_CASES)
def test_equality(test_case: str) -> None:
    generator = generate(TEST_CASES[test_case].model)
    result = units(generator)
    assert set(result) == set(TEST_CASES[test_case].expected), "unexpected or missing units"
    for filename, content in TEST_CASES[test_case].expected.items():
        assert result[filename] == content, f"mismatch in {filename}"


@pytest.mark.parametrize("test_case", TEST_CASES)
def test_compilability(test_case: str, tmp_path: Path) -> None:
    assert_compilable_code(TEST_CASES[test_case].model, tmp_path)


@pytest.mark.verification
@pytest.mark.parametrize("test_case", TEST_CASES)
def test_provability(test_case: str, tmp_path: Path) -> None:
    assert_provable_code(
        TEST_CASES[test_case].model,
        tmp_path,
        units=[
            u.replace(".ads", "")
            for u in TEST_CASES[test_case].expected
            if u.endswith(".ads") and "generic_" not in u
        ],
    )
