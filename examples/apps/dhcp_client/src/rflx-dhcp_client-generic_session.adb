pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with RFLX.DHCP; use type RFLX.DHCP.Message_Op_Code, RFLX.DHCP.Len, RFLX.DHCP.DHCP_Message_Type;
with Ada.Text_IO;

package body RFLX.DHCP_Client.Generic_Session is

   use type Types.Index, Types.Length, Types.Bit_Length;

   type Session_State is (S_Send_Discover, S_Receive_Offer, S_Send_Request, S_Receive_Ack, S_Success, S_Failure, S_Terminated);

   Discover_Ctx : DHCP_Message.Context;
   Discover_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
   Offer_Ctx : DHCP_Message.Context;
   Offer_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
   Request_Ctx : DHCP_Message.Context;
   Request_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
   Ack_Ctx : DHCP_Message.Context;
   Ack_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);

   function Send_Discover return Session_State is
      Options_Ctx : DHCP_Options.Context;
      Options_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      Option_Ctx : DHCP_Option.Context;
      Parameter_Request_List_Ctx : DHCP_Option_Codes.Context;
      Parameter_Request_List_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);

      function Valid_Length (Length : Types.Length) return Boolean is
        (Length >= 0);

      procedure Zeroed_Buffer (Buffer : out Types.Bytes)
      is
      begin
         for I in Buffer'Range loop
            Buffer (I) := Types.Byte'First;
         end loop;
      end Zeroed_Buffer;

      procedure Set_Chaddr is new DHCP_Message.Set_Chaddr (Zeroed_Buffer, Valid_Length);
      procedure Set_Sname is new DHCP_Message.Set_Sname (Zeroed_Buffer, Valid_Length);
      procedure Set_File is new DHCP_Message.Set_File (Zeroed_Buffer, Valid_Length);
   begin
      DHCP_Options.Initialize (Options_Ctx,
                               Options_Buffer,
                               Options_Buffer'First,
                               Options_Buffer'Last,
                               Types.First_Bit_Index (Options_Buffer'First),
                               Types.Last_Bit_Index (Options_Buffer'Last));

      DHCP_Options.Switch (Options_Ctx, Option_Ctx);
      DHCP_Option.Set_Code (Option_Ctx, DHCP.DHCP_MESSAGE_TYPE_OPTION);
      DHCP_Option.Set_Len (Option_Ctx, 1);
      DHCP_Option.Set_DHCP_Message_Type (Option_Ctx, DHCP.DHCPDISCOVER);
      DHCP_Options.Update (Options_Ctx, Option_Ctx);

      DHCP_Option_Codes.Initialize (Parameter_Request_List_Ctx, Parameter_Request_List_Buffer);
      DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.SUBNET_MASK_OPTION);
      DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.ROUTER_OPTION);
      DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.DOMAIN_NAME_OPTION);
      DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.DOMAIN_NAME_SERVER_OPTION);

      DHCP_Options.Switch (Options_Ctx, Option_Ctx);
      DHCP_Option.Set_Code (Option_Ctx, DHCP.PARAMETER_REQUEST_LIST_OPTION);
      DHCP_Option.Set_Len (Option_Ctx, DHCP.Len (DHCP_Option_Codes.Size (Parameter_Request_List_Ctx) / 8));
      --  declare
      --     Parameter_Request_List_Ctx : DHCP_Option_Codes.Context;
      --  begin
      --     DHCP_Option.Switch_To_Parameter_Request_List (Option_Ctx, Parameter_Request_List_Ctx);
      --     DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.SUBNET_MASK_OPTION);
      --     DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.ROUTER_OPTION);
      --     DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.DOMAIN_NAME_OPTION);
      --     DHCP_Option_Codes.Append_Element (Parameter_Request_List_Ctx, DHCP.DOMAIN_NAME_SERVER_OPTION);
      --     DHCP_Option.Update_Parameter_Request_List (Option_Ctx, Parameter_Request_List_Ctx);
      --  end;
      DHCP_Option.Set_Parameter_Request_List (Option_Ctx, Parameter_Request_List_Ctx);
      DHCP_Options.Update (Options_Ctx, Option_Ctx);

      DHCP_Options.Switch (Options_Ctx, Option_Ctx);
      DHCP_Option.Set_Code (Option_Ctx, DHCP.END_OPTION);
      DHCP_Options.Update (Options_Ctx, Option_Ctx);

      DHCP_Message.Initialize (Discover_Ctx, Discover_Buffer,
                               Types.First_Bit_Index (Discover_Buffer'First),
                               Types.First_Bit_Index (Discover_Buffer'First)
                               + 240 * 8 + DHCP_Options.Size (Options_Ctx) - 1);
      DHCP_Message.Set_Op (Discover_Ctx, DHCP.BOOTREQUEST);
      DHCP_Message.Set_Htype (Discover_Ctx, DHCP.HT_ETHERNET_10);
      DHCP_Message.Set_Hlen (Discover_Ctx, DHCP.HL_ETHERNET_10);
      DHCP_Message.Set_Hops (Discover_Ctx, 0);
      DHCP_Message.Set_Xid (Discover_Ctx, 0);
      DHCP_Message.Set_Secs (Discover_Ctx, 0);
      DHCP_Message.Set_Broadcast_Flag (Discover_Ctx, True);
      DHCP_Message.Set_Reserved_Flags (Discover_Ctx, 0);
      DHCP_Message.Set_Ciaddr (Discover_Ctx, 0);
      DHCP_Message.Set_Yiaddr (Discover_Ctx, 0);
      DHCP_Message.Set_Siaddr (Discover_Ctx, 0);
      DHCP_Message.Set_Giaddr (Discover_Ctx, 0);
      Set_Chaddr (Discover_Ctx);
      Set_Sname (Discover_Ctx);
      Set_File (Discover_Ctx);
      DHCP_Message.Set_Magic_Cookie (Discover_Ctx, 16#63825363#);
      DHCP_Message.Set_Options (Discover_Ctx, Options_Ctx);
      DHCP_Message.Take_Buffer (Discover_Ctx, Discover_Buffer);
      Send (Discover_Buffer.all
              (Discover_Ctx.Buffer_First .. Types.Byte_Index (DHCP_Message.Message_Last (Discover_Ctx))));

      return S_Receive_Offer;
   end Send_Discover;

   function Receive_Offer return Session_State is
      Offer_Message_Types_Ctx : DHCP_Client_DHCP_Message_Types.Context;
      Offer_Message_Types_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      Length : Natural;
   begin
      Ada.Text_IO.Put_Line ("Receive_Offer");
      Receive (Offer_Buffer.all, Length);
      Ada.Text_IO.Put_Line ("Received");
      DHCP_Message.Initialize (Offer_Ctx, Offer_Buffer, Types.First_Bit_Index (Offer_Buffer'First), Types.Last_Bit_Index (Offer_Buffer'First + Types.Index (Length) - 1));
      DHCP_Message.Verify_Message (Offer_Ctx);

      declare
         Options_Ctx : DHCP_Options.Context;
         Element_Ctx : DHCP_Option.Context;
         use type RFLX.DHCP.Code;
      begin
         DHCP_Client_DHCP_Message_Types.Initialize (Offer_Message_Types_Ctx, Offer_Message_Types_Buffer);
         DHCP_Message.Switch_To_Options (Offer_Ctx, Options_Ctx);
         while DHCP_Options.Has_Element (Options_Ctx) loop
            DHCP_Options.Switch (Options_Ctx, Element_Ctx);
            DHCP_Option.Verify_Message (Element_Ctx);
            if DHCP_Option.Valid (Element_Ctx, DHCP_Option.F_Code) then
               if DHCP_Option.Get_Code (Element_Ctx) = DHCP.DHCP_MESSAGE_TYPE_OPTION then
                  DHCP_Client_DHCP_Message_Types.Append_Element (Offer_Message_Types_Ctx, DHCP_Option.Get_DHCP_Message_Type (Element_Ctx));
               end if;
            end if;

            DHCP_Options.Update (Options_Ctx, Element_Ctx);
         end loop;
         DHCP_Message.Update_Options (Offer_Ctx, Options_Ctx);
      end;

      if
        DHCP_Message.Structural_Valid_Message (Offer_Ctx)
        and then DHCP_Message.Get_Op (Offer_Ctx) = DHCP.BOOTREPLY
        and then DHCP_Client_DHCP_Message_Types.Size (Offer_Message_Types_Ctx) >= DHCP.DHCP_Message_Type_Base'Size
        and then DHCP_Client_DHCP_Message_Types.Head (Offer_Message_Types_Ctx) = DHCP.DHCPOFFER
      then
         return S_Send_Request;
      end if;
      return S_Receive_Offer;
   end Receive_Offer;

   function Send_Request return Session_State is
      Options_Ctx : DHCP_Options.Context;
      Options_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      Option_Ctx     : DHCP_Option.Context;

      function Valid_Length (Length : Types.Length) return Boolean is
        (Length >= 0);

      procedure Zeroed_Buffer (Buffer : out Types.Bytes)
      is
      begin
         for I in Buffer'Range loop
            Buffer (I) := Types.Byte'First;
         end loop;
      end Zeroed_Buffer;

      procedure Set_Chaddr is new DHCP_Message.Set_Chaddr (Zeroed_Buffer, Valid_Length);
      procedure Set_Sname is new DHCP_Message.Set_Sname (Zeroed_Buffer, Valid_Length);
      procedure Set_File is new DHCP_Message.Set_File (Zeroed_Buffer, Valid_Length);
   begin
      DHCP_Options.Initialize (Options_Ctx,
                               Options_Buffer,
                               Options_Buffer'First,
                               Options_Buffer'Last,
                               Types.First_Bit_Index (Options_Buffer'First),
                               Types.Last_Bit_Index (Options_Buffer'Last));

      DHCP_Options.Switch (Options_Ctx, Option_Ctx);
      DHCP_Option.Set_Code (Option_Ctx, DHCP.DHCP_MESSAGE_TYPE_OPTION);
      DHCP_Option.Set_Len (Option_Ctx, DHCP.DHCP_Message_Type'Size / 8);
      DHCP_Option.Set_DHCP_Message_Type (Option_Ctx, DHCP.DHCPREQUEST);
      DHCP_Options.Update (Options_Ctx, Option_Ctx);

      DHCP_Options.Switch (Options_Ctx, Option_Ctx);
      DHCP_Option.Set_Code (Option_Ctx, DHCP.REQUESTED_IP_ADDRESS_OPTION);
      DHCP_Option.Set_Len (Option_Ctx, DHCP.IP_Address'Size / 8);
      DHCP_Option.Set_Requested_IP_Address (Option_Ctx, DHCP_Message.Get_Yiaddr (Offer_Ctx));
      DHCP_Options.Update (Options_Ctx, Option_Ctx);

      DHCP_Options.Switch (Options_Ctx, Option_Ctx);
      DHCP_Option.Set_Code (Option_Ctx, DHCP.SERVER_IDENTIFIER_OPTION);
      DHCP_Option.Set_Len (Option_Ctx, DHCP.IP_Address'Size / 8);
      DHCP_Option.Set_Server_Identifier (Option_Ctx, DHCP_Message.Get_Siaddr (Offer_Ctx));
      DHCP_Options.Update (Options_Ctx, Option_Ctx);

      DHCP_Message.Initialize (Request_Ctx,
                               Request_Buffer,
                               Types.First_Bit_Index (Request_Buffer'First),
                               Types.First_Bit_Index (Request_Buffer'First)
                               + 240 * 8 + DHCP_Options.Size (Options_Ctx) - 1);
      DHCP_Message.Set_Op (Request_Ctx, DHCP.BOOTREQUEST);
      DHCP_Message.Set_Htype (Request_Ctx, DHCP.HT_ETHERNET_10);
      DHCP_Message.Set_Hlen (Request_Ctx, DHCP.HL_ETHERNET_10);
      DHCP_Message.Set_Hops (Request_Ctx, 0);
      DHCP_Message.Set_Xid (Request_Ctx, 0);
      DHCP_Message.Set_Secs (Request_Ctx, 0);
      DHCP_Message.Set_Broadcast_Flag (Request_Ctx, True);
      DHCP_Message.Set_Reserved_Flags (Request_Ctx, 0);
      DHCP_Message.Set_Ciaddr (Request_Ctx, DHCP_Message.Get_Yiaddr (Offer_Ctx));
      DHCP_Message.Set_Yiaddr (Request_Ctx, 0);
      DHCP_Message.Set_Siaddr (Request_Ctx, DHCP_Message.Get_Siaddr (Offer_Ctx));
      DHCP_Message.Set_Giaddr (Request_Ctx, 0);
      Set_Chaddr (Request_Ctx);
      Set_Sname (Request_Ctx);
      Set_File (Request_Ctx);
      DHCP_Message.Set_Magic_Cookie (Request_Ctx, 16#63825363#);
      DHCP_Message.Set_Options (Request_Ctx, Options_Ctx);
      DHCP_Message.Take_Buffer (Request_Ctx, Request_Buffer);

      Send (Request_Buffer.all
              (Request_Ctx.Buffer_First .. Types.Byte_Index (DHCP_Message.Message_Last (Request_Ctx))));

      return S_Receive_Ack;
   end Send_Request;

   function Receive_Ack return Session_State is
      Ack_Message_Types_Ctx : DHCP_Client_DHCP_Message_Types.Context;
      Ack_Message_Types_Buffer : Types.Bytes_Ptr := new Types.Bytes'(Types.Index'First .. Types.Index'First + 4095 => Types.Byte'First);
      Length : Natural;
   begin
      Ada.Text_IO.Put_Line ("Receive_Ack");
      Receive (Ack_Buffer.all, Length);
      Ada.Text_IO.Put_Line ("Received");
      DHCP_Message.Initialize (Ack_Ctx, Ack_Buffer, Types.First_Bit_Index (Ack_Buffer'First), Types.Last_Bit_Index (Ack_Buffer'First + Types.Index (Length) - 1));
      DHCP_Message.Verify_Message (Ack_Ctx);

      declare
         Options_Ctx : DHCP_Options.Context;
         Element_Ctx : DHCP_Option.Context;
         use type RFLX.DHCP.Code;
      begin
         DHCP_Client_DHCP_Message_Types.Initialize (Ack_Message_Types_Ctx, Ack_Message_Types_Buffer);
         DHCP_Message.Switch_To_Options (Ack_Ctx, Options_Ctx);
         while DHCP_Options.Has_Element (Options_Ctx) loop
            DHCP_Options.Switch (Options_Ctx, Element_Ctx);
            DHCP_Option.Verify_Message (Element_Ctx);
            if DHCP_Option.Valid (Element_Ctx, DHCP_Option.F_Code) then
               if DHCP_Option.Get_Code (Element_Ctx) = DHCP.DHCP_MESSAGE_TYPE_OPTION then
                  DHCP_Client_DHCP_Message_Types.Append_Element (Ack_Message_Types_Ctx, DHCP_Option.Get_DHCP_Message_Type (Element_Ctx));
               end if;
            end if;

            DHCP_Options.Update (Options_Ctx, Element_Ctx);
         end loop;
         DHCP_Message.Update_Options (Ack_Ctx, Options_Ctx);
      end;

      if
        DHCP_Message.Structural_Valid_Message (Ack_Ctx)
        and then DHCP_Message.Get_Op (Ack_Ctx) = DHCP.BOOTREPLY
        and then DHCP_Client_DHCP_Message_Types.Size (Ack_Message_Types_Ctx) > 0
        and then DHCP_Client_DHCP_Message_Types.Head (Ack_Message_Types_Ctx) = DHCP.DHCPACK
      then
         return S_Success;
      end if;
      return S_Failure;
   end Receive_Ack;

   function Success return Session_State is
   begin
      Ada.Text_IO.Put_Line ("Success");
      return S_Terminated;
   end Success;

   function Failure return Session_State is
   begin
      Ada.Text_IO.Put_Line ("Failure");
      return S_Terminated;
   end Failure;

   procedure Run is
      State : Session_State := S_Send_Discover;
   begin
      while State /= S_Terminated loop
         case State is
            when S_Send_Discover =>
               State := Send_Discover;
            when S_Receive_Offer =>
               State := Receive_Offer;
            when S_Send_Request =>
               State := Send_Request;
            when S_Receive_Ack =>
               State := Receive_Ack;
            when S_Success =>
               State := Success;
            when S_Failure =>
               State := Failure;
            when S_Terminated =>
               null;
         end case;
      end loop;
   end Run;

end RFLX.DHCP_Client.Generic_Session;
