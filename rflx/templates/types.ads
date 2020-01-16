with System.Storage_Elements;

package {prefix}Types with
  SPARK_Mode
is

   type Byte is mod 2**8;

   type Length is new Natural;
   subtype Index is Length range 1 .. Length'Last;

   type Bit_Length is range 0 .. 2**34 - 8;
   subtype Bit_Index is Bit_Length range 1 .. Bit_Length'Last;

   function Byte_Index (Bit_Idx : Bit_Index) return Index is
     (Length ((Bit_Idx - 1) / 8) + 1);

   function First_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 1);

   function Last_Bit_Index (Idx : Index) return Bit_Index is
     ((Bit_Length (Idx) - 1) * 8 + 8);

   type Bytes is array (Index range <>) of Byte;

   type Bytes_Ptr is access Bytes;

   type Integer_Address is new System.Storage_Elements.Integer_Address;

   function Bytes_Address (Buffer : Bytes_Ptr) return Integer_Address with
     Global => null;

   function Bytes_First (Buffer : access constant Bytes) return Index is
     (Buffer'First)
     with
       Pre => Buffer /= null and then Buffer'Length > 0;

   function Bytes_Last (Buffer : access constant Bytes) return Index is
     (Buffer'Last)
     with
       Pre => Buffer /= null and then Buffer'Length > 0;

   type Offset is mod 8;

   generic
      type Index_Type   is (<>);
      type Element_Type is (<>);
      type Array_Type   is array (Index_Type range <>) of Element_Type;
      type Offset_Type  is (<>);
      type Value_Type   is (<>);
   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type with
     Pre => (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size < Data'Length
             and then (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size <= Natural'Size
             and then Natural (((Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size) * Element_Type'Size) < Long_Integer'Size - 1
             and then (Element_Type'Size - Natural (Offset_Type'Pos (Offset) mod Element_Type'Size)) < Long_Integer'Size - 1;

   generic
      type Index_Type   is (<>);
      type Element_Type is (<>);
      type Array_Type   is array (Index_Type range <>) of Element_Type;
      type Offset_Type  is (<>);
      type Value_Type   is (<>);
   procedure Insert (Value  :        Value_Type;
                     Data   : in out Array_Type;
                     Offset :        Offset_Type) with
     Pre => (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size < Data'Length;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Bit_Length return Bit_Length is
     (Bit_Length'First)
    with
     Pre => False;

   pragma Warnings (On, "precondition is statically false");

end {prefix}Types;
