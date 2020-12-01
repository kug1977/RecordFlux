with GNAT.Sockets;

--  with RFLX.RFLX_Builtin_Types;
with RFLX.RFLX_Types;
with RFLX.DHCP.Message;
with RFLX.DHCP.Option;
with RFLX.DHCP.Options;
with RFLX.DHCP.Cookie_Servers;
with RFLX.DHCP.Domain_Name_Servers;
with RFLX.DHCP.Impress_Servers;
with RFLX.DHCP.LPR_Servers;
with RFLX.DHCP.Log_Servers;
with RFLX.DHCP.Name_Servers;
with RFLX.DHCP.Option_Codes;
with RFLX.DHCP.Resource_Location_Servers;
with RFLX.DHCP.Routers;
with RFLX.DHCP.Time_Servers;
with RFLX.DHCP_Client.Generic_Session;
with RFLX.DHCP_Client.DHCP_Message_Types;
with RFLX.DHCP.NetBIOS_Over_TCP_IP_Datagram_Distribution_Servers;
with RFLX.DHCP.NetBIOS_Over_TCP_IP_Name_Servers;
with RFLX.DHCP.Network_Information_Servers;
with RFLX.DHCP.Network_Time_Protocol_Servers;
with RFLX.DHCP.Path_MTU_Plateau_Table;
with RFLX.DHCP.Static_Route;
with RFLX.DHCP.Static_Routes;
with RFLX.DHCP.X_Window_System_Display_Managers;
with RFLX.DHCP.X_Window_System_Font_Servers;

with Channel;

procedure DHCP_Client is
   Client_Send : GNAT.Sockets.Socket_Type;
   Client_Receive : GNAT.Sockets.Socket_Type;
begin
   GNAT.Sockets.Create_Socket (Socket => Client_Send,
                               Family => GNAT.Sockets.Family_Inet,
                               Mode => GNAT.Sockets.Socket_Datagram);
   GNAT.Sockets.Set_Socket_Option (Socket => Client_Send,
                                   Level => GNAT.Sockets.Socket_Level,
                                   Option => (GNAT.Sockets.Broadcast, True));
   GNAT.Sockets.Create_Socket (Socket => Client_Receive,
                               Family => GNAT.Sockets.Family_Inet,
                               Mode => GNAT.Sockets.Socket_Datagram);
   GNAT.Sockets.Set_Socket_Option (Socket => Client_Receive,
                                   Level => GNAT.Sockets.Socket_Level,
                                   Option => (GNAT.Sockets.Broadcast, True));
   GNAT.Sockets.Set_Socket_Option (Socket => Client_Receive,
                                   Level => GNAT.Sockets.Socket_Level,
                                   Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                              Timeout => GNAT.Sockets.Timeval_Duration (5)));
   GNAT.Sockets.Bind_Socket (Socket  => Client_Receive,
                             Address => (Family => GNAT.Sockets.Family_Inet,
                                         Addr => GNAT.Sockets.Any_Inet_Addr,
                                         Port => 68));
   declare
      package Client_Channel is new Channel (Client_Send, Client_Receive);
      package Session is new RFLX.DHCP_Client.Generic_Session
        (RFLX.RFLX_Types,
         Client_Channel.Send,
         Client_Channel.Receive,
         RFLX.DHCP.Cookie_Servers,
         RFLX.DHCP.Domain_Name_Servers,
         RFLX.DHCP.Impress_Servers,
         RFLX.DHCP.LPR_Servers,
         RFLX.DHCP.Log_Servers,
         RFLX.DHCP.Name_Servers,
         RFLX.DHCP.Option_Codes,
         RFLX.DHCP.Resource_Location_Servers,
         RFLX.DHCP.Routers,
         RFLX.DHCP.Time_Servers,
         RFLX.DHCP.NetBIOS_Over_TCP_IP_Datagram_Distribution_Servers,
         RFLX.DHCP.NetBIOS_Over_TCP_IP_Name_Servers,
         RFLX.DHCP.Network_Information_Servers,
         RFLX.DHCP.Network_Time_Protocol_Servers,
         RFLX.DHCP.Path_MTU_Plateau_Table,
         RFLX.DHCP.Static_Route,
         RFLX.DHCP.Static_Routes,
         RFLX.DHCP.X_Window_System_Display_Managers,
         RFLX.DHCP.X_Window_System_Font_Servers,
         RFLX.DHCP.Option,
         RFLX.DHCP.Options,
         RFLX.DHCP.Message,
         RFLX.DHCP_Client.DHCP_Message_Types);
   begin
      Session.Run;
   end;
   GNAT.Sockets.Close_Socket (Client_Send);
   GNAT.Sockets.Close_Socket (Client_Receive);
end DHCP_Client;
