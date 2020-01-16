with RFLX.Types;
use type RFLX.Types.Integer_Address;

generic
package RFLX.UDP.Generic_Datagram with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Datagram);

   type Virtual_Field is (F_Initial, F_Source_Port, F_Destination_Port, F_Length, F_Checksum, F_Payload, F_Final);

   subtype Field is Virtual_Field range F_Source_Port .. F_Payload;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Payload | F_Final =>
               null;
            when F_Source_Port =>
               Source_Port_Value : Port;
            when F_Destination_Port =>
               Destination_Port_Value : Port;
            when F_Length =>
               Length_Value : Length_Base;
            when F_Checksum =>
               Checksum_Value : Checksum;
         end case;
      end record;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= RFLX.Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Buffer = null
          and Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and Ctx.First = RFLX.Types.First_Bit_Index (Ctx.Buffer_First)
          and Initialized (Ctx);

   procedure Initialize (Ctx : out Context; Buffer : in out RFLX.Types.Bytes_Ptr; First, Last : RFLX.Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then RFLX.Types.Byte_Index (First) >= Buffer'First
          and then RFLX.Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= RFLX.Types.Bit_Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Buffer = null
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = RFLX.Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = RFLX.Types.Bytes_Last (Buffer)'Old
          and Ctx.First = First
          and Ctx.Last = Last
          and Initialized (Ctx);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out RFLX.Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Buffer /= null
          and Ctx.Buffer_First = Buffer'First
          and Ctx.Buffer_Last = Buffer'Last
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old
          and Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Value : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Value.Fld in Field'Range
          and Valid_Predecessor (Ctx, Value.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return RFLX.Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       (if Valid'Result then
           Structural_Valid (Ctx, Fld)
             and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Invalid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Get_Source_Port (Ctx : Context) return Port with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Source_Port);

   function Get_Destination_Port (Ctx : Context) return Port with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Destination_Port);

   function Get_Length (Ctx : Context) return Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Length);

   function Get_Checksum (Ctx : Context) return Checksum with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Checksum);

   generic
      with procedure Process_Payload (Payload : RFLX.Types.Bytes);
   procedure Get_Payload (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Payload);

   procedure Set_Source_Port (Ctx : in out Context; Value : Port) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Source_Port)
          and then Field_Last (Ctx, F_Source_Port) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Source_Port, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Source_Port) >= Field_Length (Ctx, F_Source_Port),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Source_Port)
          and Get_Source_Port (Ctx) = Value
          and Invalid (Ctx, F_Destination_Port)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Checksum)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Destination_Port) = F_Source_Port
            and Valid_Next (Ctx, F_Destination_Port))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Source_Port) = Predecessor (Ctx, F_Source_Port)'Old
          and Valid_Next (Ctx, F_Source_Port) = Valid_Next (Ctx, F_Source_Port)'Old;

   procedure Set_Destination_Port (Ctx : in out Context; Value : Port) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Destination_Port)
          and then Field_Last (Ctx, F_Destination_Port) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Destination_Port, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Destination_Port) >= Field_Length (Ctx, F_Destination_Port),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Destination_Port)
          and Get_Destination_Port (Ctx) = Value
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Checksum)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Length) = F_Destination_Port
            and Valid_Next (Ctx, F_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Destination_Port) = Predecessor (Ctx, F_Destination_Port)'Old
          and Valid_Next (Ctx, F_Destination_Port) = Valid_Next (Ctx, F_Destination_Port)'Old
          and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
          and Cursor (Ctx, F_Source_Port) = Cursor (Ctx, F_Source_Port)'Old;

   procedure Set_Length (Ctx : in out Context; Value : Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Length)
          and then Field_Last (Ctx, F_Length) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Length, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Length) >= Field_Length (Ctx, F_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Length)
          and Get_Length (Ctx) = Value
          and Invalid (Ctx, F_Checksum)
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Checksum) = F_Length
            and Valid_Next (Ctx, F_Checksum))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
          and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old
          and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
          and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
          and Cursor (Ctx, F_Source_Port) = Cursor (Ctx, F_Source_Port)'Old
          and Cursor (Ctx, F_Destination_Port) = Cursor (Ctx, F_Destination_Port)'Old;

   procedure Set_Checksum (Ctx : in out Context; Value : Checksum) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Checksum)
          and then Field_Last (Ctx, F_Checksum) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Checksum, Value))
          and then Valid (Value)
          and then Available_Space (Ctx, F_Checksum) >= Field_Length (Ctx, F_Checksum),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Checksum)
          and Get_Checksum (Ctx) = Value
          and Invalid (Ctx, F_Payload)
          and (Predecessor (Ctx, F_Payload) = F_Checksum
            and Valid_Next (Ctx, F_Payload))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Checksum) = Predecessor (Ctx, F_Checksum)'Old
          and Valid_Next (Ctx, F_Checksum) = Valid_Next (Ctx, F_Checksum)'Old
          and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
          and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
          and Get_Length (Ctx) = Get_Length (Ctx)'Old
          and Cursor (Ctx, F_Source_Port) = Cursor (Ctx, F_Source_Port)'Old
          and Cursor (Ctx, F_Destination_Port) = Cursor (Ctx, F_Destination_Port)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old;

   generic
      with procedure Process_Payload (Payload : out RFLX.Types.Bytes);
   procedure Set_Payload (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Payload)
          and then Field_Last (Ctx, F_Payload) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Payload))
          and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
          and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
          and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
          and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
          and Get_Length (Ctx) = Get_Length (Ctx)'Old
          and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
          and Structural_Valid (Ctx, F_Payload);

   procedure Initialize_Payload (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Payload)
          and then Field_Last (Ctx, F_Payload) <= RFLX.Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Payload))
          and then Available_Space (Ctx, F_Payload) >= Field_Length (Ctx, F_Payload),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Payload) = Predecessor (Ctx, F_Payload)'Old
          and Valid_Next (Ctx, F_Payload) = Valid_Next (Ctx, F_Payload)'Old
          and Get_Source_Port (Ctx) = Get_Source_Port (Ctx)'Old
          and Get_Destination_Port (Ctx) = Get_Destination_Port (Ctx)'Old
          and Get_Length (Ctx) = Get_Length (Ctx)'Old
          and Get_Checksum (Ctx) = Get_Checksum (Ctx)'Old
          and Structural_Valid (Ctx, F_Payload);

   function Valid_Context (Ctx : Context) return Boolean with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Value : Field_Dependent_Value) return Boolean is
     ((case Value.Fld is
         when F_Source_Port =>
            Valid (Value.Source_Port_Value),
         when F_Destination_Port =>
            Valid (Value.Destination_Port_Value),
         when F_Length =>
            Valid (Value.Length_Value),
         when F_Checksum =>
            Valid (Value.Checksum_Value),
         when F_Payload =>
            True,
         when F_Initial | F_Final =>
            False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First;
               Last : RFLX.Types.Bit_Length := RFLX.Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
             or State = S_Structural_Valid then
           Valid_Value (Value));

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   function Valid_Context (Buffer_First, Buffer_Last : RFLX.Types.Index; First, Last : RFLX.Types.Bit_Index; Buffer : access constant RFLX.Types.Bytes; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last)
      and then RFLX.Types.Byte_Index (First) >= Buffer_First
      and then RFLX.Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= RFLX.Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and Cursors (F).Last <= Last
           and Cursors (F).First <= (Cursors (F).Last + 1)
           and Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Destination_Port)) then
           (Valid (Cursors (F_Source_Port))
               and then Cursors (F_Destination_Port).Predecessor = F_Source_Port))
        and then (if Structural_Valid (Cursors (F_Length)) then
           (Valid (Cursors (F_Destination_Port))
               and then Cursors (F_Length).Predecessor = F_Destination_Port))
        and then (if Structural_Valid (Cursors (F_Checksum)) then
           (Valid (Cursors (F_Length))
               and then Cursors (F_Checksum).Predecessor = F_Length))
        and then (if Structural_Valid (Cursors (F_Payload)) then
           (Valid (Cursors (F_Checksum))
               and then Cursors (F_Payload).Predecessor = F_Checksum)))
      and then ((if Invalid (Cursors (F_Source_Port)) then
           Invalid (Cursors (F_Destination_Port)))
        and then (if Invalid (Cursors (F_Destination_Port)) then
           Invalid (Cursors (F_Length)))
        and then (if Invalid (Cursors (F_Length)) then
           Invalid (Cursors (F_Checksum)))
        and then (if Invalid (Cursors (F_Checksum)) then
           Invalid (Cursors (F_Payload))))
      and then (if Structural_Valid (Cursors (F_Source_Port)) then
         (Cursors (F_Source_Port).Last - Cursors (F_Source_Port).First + 1) = UDP.Port'Size
           and then Cursors (F_Source_Port).Predecessor = F_Initial
           and then Cursors (F_Source_Port).First = First
           and then (if Structural_Valid (Cursors (F_Destination_Port)) then
              (Cursors (F_Destination_Port).Last - Cursors (F_Destination_Port).First + 1) = UDP.Port'Size
                and then Cursors (F_Destination_Port).Predecessor = F_Source_Port
                and then Cursors (F_Destination_Port).First = (Cursors (F_Source_Port).Last + 1)
                and then (if Structural_Valid (Cursors (F_Length)) then
                   (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = UDP.Length_Base'Size
                     and then Cursors (F_Length).Predecessor = F_Destination_Port
                     and then Cursors (F_Length).First = (Cursors (F_Destination_Port).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Checksum)) then
                        (Cursors (F_Checksum).Last - Cursors (F_Checksum).First + 1) = UDP.Checksum'Size
                          and then Cursors (F_Checksum).Predecessor = F_Length
                          and then Cursors (F_Checksum).First = (Cursors (F_Length).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Payload)) then
                             (Cursors (F_Payload).Last - Cursors (F_Payload).First + 1) = ((RFLX.Types.Bit_Length (Cursors (F_Length).Value.Length_Value) - 8)) * 8
                               and then Cursors (F_Payload).Predecessor = F_Checksum
                               and then Cursors (F_Payload).First = (Cursors (F_Checksum).Last + 1)))))));

   type Context (Buffer_First, Buffer_Last : RFLX.Types.Index := RFLX.Types.Index'First; First, Last : RFLX.Types.Bit_Index := RFLX.Types.Bit_Index'First) is
      record
         Buffer : RFLX.Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Buffer_First, Buffer_Last, First, Last, Buffer, Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end RFLX.UDP.Generic_Datagram;
