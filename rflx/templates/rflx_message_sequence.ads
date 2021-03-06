pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
with {prefix}RFLX_Generic_Types;

generic
   with package Types is new {prefix}RFLX_Generic_Types (<>);
   type Element_Context (Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) is private;
   with procedure Element_Initialize (Ctx : out Element_Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index);
   with procedure Element_Take_Buffer (Ctx : in out Element_Context; Buffer : out Types.Bytes_Ptr);
   with function Element_Has_Buffer (Ctx : Element_Context) return Boolean;
   with function Element_Last (Ctx : Element_Context) return Types.Bit_Index;
   with function Element_Initialized (Ctx : Element_Context) return Boolean;
   with function Element_Valid_Message (Ctx : Element_Context) return Boolean;
package {prefix}RFLX_Message_Sequence with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, RFLX_Message_Sequence);

   pragma Unevaluated_Use_Of_Old (Allow);

   use type Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       Types.Byte_Index (First) >= Buffer_First
       and Types.Byte_Index (Last) <= Buffer_Last
       and First mod Types.Byte'Size = 1
       and First <= Last
       and Last <= Types.Bit_Index'Last - 1;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'Length > 0
        and then Buffer'Last < Types.Index'Last),
     Post =>
       (Has_Buffer (Ctx)
        and Valid (Ctx)
        and Buffer = null
        and Ctx.Buffer_First = Buffer'First'Old
        and Ctx.Buffer_Last = Buffer'Last'Old
        and Ctx.First = Types.First_Bit_Index (Ctx.Buffer_First)
        and Ctx.Last = Types.Last_Bit_Index (Ctx.Buffer_Last)
        and Sequence_Last (Ctx) = Ctx.First - 1),
     Depends =>
       (Ctx => Buffer, Buffer => null);

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index) with
     Pre =>
       (not Ctx'Constrained
        and then Buffer /= null
        and then Buffer'First = Buffer_First
        and then Buffer'Last = Buffer_Last
        and then Types.Byte_Index (First) >= Buffer'First
        and then Types.Byte_Index (Last) <= Buffer'Last
        and then First mod Types.Byte'Size = 1
        and then First <= Last
        and then Last <= Types.Bit_Index'Last - 1),
     Post =>
       (Buffer = null
        and Has_Buffer (Ctx)
        and Valid (Ctx)
        and Ctx.Buffer_First = Buffer_First
        and Ctx.Buffer_Last = Buffer_Last
        and Ctx.First = First
        and Ctx.Last = Last
        and Sequence_Last (Ctx) = First - 1),
     Depends =>
       (Ctx => (Buffer, Buffer_First, Buffer_Last, First, Last), Buffer => null);

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) with
     Pre =>
       Has_Buffer (Ctx),
     Post =>
       (not Has_Buffer (Ctx)
        and Buffer /= null
        and Buffer'First = Ctx.Buffer_First
        and Buffer'Last = Ctx.Buffer_Last
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Valid (Ctx) = Valid (Ctx)'Old
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old),
     Depends =>
       (Ctx => Ctx, Buffer => Ctx);

   procedure Copy (Ctx : Context; Buffer : out Types.Bytes) with
     Pre =>
       (Has_Buffer (Ctx)
        and Byte_Size (Ctx) = Buffer'Length);

   function Has_Element (Ctx : Context) return Boolean with
     Contract_Cases =>
       (Has_Buffer (Ctx) => (Has_Element'Result or not Has_Element'Result) and Has_Buffer (Ctx),
        not Has_Buffer (Ctx) => (Has_Element'Result or not Has_Element'Result) and not Has_Buffer (Ctx));

   procedure Switch (Ctx : in out Context; Element_Ctx : out Element_Context) with
     Pre =>
       (not Element_Ctx'Constrained
        and then Has_Buffer (Ctx)
        and then Has_Element (Ctx)
        and then Valid (Ctx)),
     Post =>
       (not Has_Buffer (Ctx)
        and Has_Element (Ctx)
        and Valid (Ctx)
        and Element_Has_Buffer (Element_Ctx)
        and Ctx.Buffer_First = Element_Ctx.Buffer_First
        and Ctx.Buffer_Last = Element_Ctx.Buffer_Last
        and Ctx.First <= Element_Ctx.First
        and Ctx.Last >= Element_Ctx.Last
        and Element_Ctx.First = Sequence_Last (Ctx) + 1
        and Element_Ctx.Last = Ctx.Last
        and Element_Initialized (Element_Ctx)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old
        and Sequence_Last (Ctx) = Sequence_Last (Ctx)'Old),
     Depends =>
       (Ctx => Ctx, Element_Ctx => Ctx);

   procedure Update (Ctx : in out Context; Element_Ctx : in out Element_Context) with
     Pre =>
       (not Has_Buffer (Ctx)
        and then Element_Has_Buffer (Element_Ctx)
        and then Has_Element (Ctx)
        and then Valid (Ctx)
        and then Ctx.Buffer_First = Element_Ctx.Buffer_First
        and then Ctx.Buffer_Last = Element_Ctx.Buffer_Last
        and then Ctx.First <= Element_Ctx.First
        and then Ctx.Last >= Element_Ctx.Last),
     Post =>
       (Has_Buffer (Ctx)
        and not Element_Has_Buffer (Element_Ctx)
        and (if Element_Valid_Message (Element_Ctx)'Old then Valid (Ctx))
        and Sequence_Last (Ctx) = (if Element_Valid_Message (Element_Ctx)'Old then Element_Last (Element_Ctx)'Old else Sequence_Last (Ctx)'Old)
        and Ctx.Buffer_First = Ctx.Buffer_First'Old
        and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
        and Ctx.First = Ctx.First'Old
        and Ctx.Last = Ctx.Last'Old),
     Contract_Cases =>
       (Element_Valid_Message (Element_Ctx) =>
          (Sequence_Last (Ctx) = Element_Last (Element_Ctx)'Old),
        others =>
          True),
     Depends =>
       (Ctx => (Ctx, Element_Ctx), Element_Ctx => Element_Ctx);

   function Valid (Ctx : Context) return Boolean;

   function Has_Buffer (Ctx : Context) return Boolean;

   function Sequence_Last (Ctx : Context) return Types.Bit_Length with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Size (Ctx : Context) return Types.Bit_Length with
     Annotate =>
       (GNATprove, Inline_For_Proof);

   function Byte_Size (Ctx : Context) return Types.Length with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Context_State is (S_Valid, S_Invalid);

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Sequence_Last : Types.Bit_Length := First - 1;
         Buffer        : Types.Bytes_Ptr := null;
         State         : Context_State := S_Valid;
      end record with
     Dynamic_Predicate =>
       ((if Buffer /= null then
          (Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last))
        and Types.Byte_Index (First) >= Buffer_First
        and Types.Byte_Index (Last) <= Buffer_Last
        and First mod Types.Byte'Size = 1
        and First <= Last
        and Last <= Types.Bit_Index'Last - 1
        and Sequence_Last >= First - 1
        and Sequence_Last <= Last);

   function Sequence_Last (Ctx : Context) return Types.Bit_Length is
      (Ctx.Sequence_Last);

   function Size (Ctx : Context) return Types.Bit_Length is
      (Ctx.Sequence_Last - Ctx.First + 1);

   function Byte_Size (Ctx : Context) return Types.Length is
     (if
        Ctx.Sequence_Last = Ctx.First - 1
      then
         0
      else
         Types.Length (Types.Byte_Index (Ctx.Sequence_Last) - Types.Byte_Index (Ctx.First)) + 1);

end {prefix}RFLX_Message_Sequence;
