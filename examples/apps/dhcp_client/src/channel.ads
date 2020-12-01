with GNAT.Sockets;
with RFLX.RFLX_Builtin_Types;

generic
   Send_Socket : GNAT.Sockets.Socket_Type;
   Receive_Socket : GNAT.Sockets.Socket_Type;
package Channel is

   procedure Send (Buffer : RFLX.RFLX_Builtin_Types.Bytes);

   procedure Receive (Buffer : in out RFLX.RFLX_Builtin_Types.Bytes; Length : out Natural);

end Channel;
