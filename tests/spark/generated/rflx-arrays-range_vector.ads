pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Scalar_Sequence;
with RFLX.Arrays;
with RFLX.RFLX_Types;

package RFLX.Arrays.Range_Vector is new RFLX.RFLX_Scalar_Sequence (RFLX.RFLX_Types, RFLX.Arrays.Range_Integer, RFLX.Arrays.Range_Integer_Base, RFLX.Arrays.Valid, RFLX.Arrays.To_Actual, RFLX.Arrays.To_Base);
