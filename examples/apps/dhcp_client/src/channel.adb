with Ada.Streams;
with Ada.Text_IO;

package body Channel is

   use type RFLX.RFLX_Builtin_Types.Length;
   use type Ada.Streams.Stream_Element_Offset;

   function To_Ada_Stream (Buffer : RFLX.RFLX_Builtin_Types.Bytes) return Ada.Streams.Stream_Element_Array with
     Pre => Buffer'First = 1
   is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Data (Ada.Streams.Stream_Element_Offset (I)) := Ada.Streams.Stream_Element (Buffer (I));
      end loop;
      return Data;
   end To_Ada_Stream;

   function To_RFLX_Bytes (Buffer : Ada.Streams.Stream_Element_Array) return RFLX.RFLX_Builtin_Types.Bytes with
     Pre => Buffer'First = 1
   is
      Data : RFLX.RFLX_Builtin_Types.Bytes (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Data (RFLX.RFLX_Builtin_Types.Length (I)) := RFLX.RFLX_Builtin_Types.Byte (Buffer (I));
      end loop;
      return Data;
   end To_RFLX_Bytes;

   procedure Send (Buffer : RFLX.RFLX_Builtin_Types.Bytes)
   is
      Data : constant Ada.Streams.Stream_Element_Array (1 .. Buffer'Length) := To_Ada_Stream (Buffer);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket (Socket => Send_Socket,
                                Item => Data,
                                Last => Last,
                                To => GNAT.Sockets.Sock_Addr_Type'(Family => GNAT.Sockets.Family_Inet,
                                                                   Addr => GNAT.Sockets.Inet_Addr ("255.255.255.255"),
                                                                   Port => 67));
      Ada.Text_IO.Put_Line ("Send Last:" & Last'Img);
   end Send;

   procedure Receive (Buffer : in out RFLX.RFLX_Builtin_Types.Bytes; Length : out Natural) is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Text_IO.Put_Line ("Receive");
      GNAT.Sockets.Receive_Socket (Socket => Receive_Socket,
                                   Item => Data,
                                   Last => Last);
      Ada.Text_IO.Put_Line ("Receive Last:" & Last'Img);
      Buffer := To_RFLX_Bytes (Data);
      Length := Natural (Last);
   end Receive;

end Channel;
