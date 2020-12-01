pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
with RFLX.RFLX_Generic_Types;
with RFLX.DHCP.Generic_Message;
with RFLX.DHCP.Generic_Option;
with RFLX.DHCP.Generic_Static_Route;
with RFLX.RFLX_Message_Sequence;
with RFLX.RFLX_Scalar_Sequence;

generic
   with package Types is new RFLX.RFLX_Generic_Types (<>);
   with procedure Send (Buffer : Types.Bytes);
   with procedure Receive (Buffer : in out Types.Bytes; Length : out Natural);
   with package DHCP_Cookie_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Domain_Name_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Impress_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_LPR_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Log_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Name_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Option_Codes is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.Code, RFLX.DHCP.Code_Base, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Resource_Location_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Routers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Time_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_NetBIOS_Over_TCP_IP_Datagram_Distribution_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_NetBIOS_Over_TCP_IP_Name_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Network_Information_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Network_Time_Protocol_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Path_MTU_Plateau_Table is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.MTU, RFLX.DHCP.MTU_Base, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Static_Route is new RFLX.DHCP.Generic_Static_Route (Types);
   with package DHCP_Static_Routes is new RFLX.RFLX_Message_Sequence (Types, DHCP_Static_Route.Context, DHCP_Static_Route.Initialize, DHCP_Static_Route.Take_Buffer, DHCP_Static_Route.Has_Buffer, DHCP_Static_Route.Message_Last, DHCP_Static_Route.Initialized, DHCP_Static_Route.Structural_Valid_Message);
   with package DHCP_X_Window_System_Display_Managers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_X_Window_System_Font_Servers is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.IP_Address, RFLX.DHCP.IP_Address, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
   with package DHCP_Option is new RFLX.DHCP.Generic_Option (Types, DHCP_Cookie_Servers, DHCP_Domain_Name_Servers, DHCP_Impress_Servers, DHCP_LPR_Servers, DHCP_Log_Servers, DHCP_Name_Servers, DHCP_NetBIOS_Over_TCP_IP_Datagram_Distribution_Servers, DHCP_NetBIOS_Over_TCP_IP_Name_Servers, DHCP_Network_Information_Servers, DHCP_Network_Time_Protocol_Servers, DHCP_Option_Codes, DHCP_Path_MTU_Plateau_Table, DHCP_Resource_Location_Servers, DHCP_Routers, DHCP_Static_Routes, DHCP_Time_Servers, DHCP_X_Window_System_Display_Managers, DHCP_X_Window_System_Font_Servers);
   with package DHCP_Options is new RFLX.RFLX_Message_Sequence (Types, DHCP_Option.Context, DHCP_Option.Initialize, DHCP_Option.Take_Buffer, DHCP_Option.Has_Buffer, DHCP_Option.Message_Last, DHCP_Option.Initialized, DHCP_Option.Structural_Valid_Message);
   with package DHCP_Message is new RFLX.DHCP.Generic_Message (Types, DHCP_Options);
   with package DHCP_Client_DHCP_Message_Types is new RFLX.RFLX_Scalar_Sequence (Types, RFLX.DHCP.DHCP_Message_Type, RFLX.DHCP.DHCP_Message_Type_Base, RFLX.DHCP.Valid, RFLX.DHCP.To_Actual, RFLX.DHCP.To_Base);
package RFLX.DHCP_Client.Generic_Session with
  SPARK_Mode,
  Annotate =>
    (GNATprove, Terminating)
is

   procedure Run;

end RFLX.DHCP_Client.Generic_Session;
