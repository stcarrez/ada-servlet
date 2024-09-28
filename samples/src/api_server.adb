-----------------------------------------------------------------------
--  api_server -- Example of REST API server
--  Copyright (C) 2016, 2018, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Server;
with Servlet.Core.Rest;
with Servlet.Core.Files;
with Servlet.Rest;
with Util.Log.Loggers;
with Monitor;

procedure API_Server is
   CONFIG_PATH  : constant String := "samples.properties";

   Api     : aliased Servlet.Core.Rest.Rest_Servlet;
   Files   : aliased Servlet.Core.Files.File_Servlet;
   App     : aliased Servlet.Core.Servlet_Registry;
   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Api_Server");
begin
   Util.Log.Loggers.Initialize (CONFIG_PATH);

   App.Set_Init_Parameter (Servlet.Core.Files.VIEW_DIR_PARAM, "samples/web/monitor");

   --  Register the servlets and filters
   App.Add_Servlet (Name => "api", Server => Api'Unchecked_Access);
   App.Add_Servlet (Name => "files", Server => Files'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "api", Pattern => "/api/*");
   App.Add_Mapping (Name => "files", Pattern => "*.html");
   App.Add_Mapping (Name => "files", Pattern => "*.css");
   App.Add_Mapping (Name => "files", Pattern => "*.js");

   Servlet.Rest.Register (App, Monitor.API_Get_Values.Definition);
   Servlet.Rest.Register (App, Monitor.API_Put_Value.Definition);

   Server.WS.Register_Application ("/monitor", App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:8080/monitor/index.html");

   Server.WS.Start;

   delay 6000.0;

end API_Server;