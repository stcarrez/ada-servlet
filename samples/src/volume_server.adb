-----------------------------------------------------------------------
--  volume_server -- Example of server with a servlet
--  Copyright (C) 2010, 2015, 2018, 2021, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Server;
with Server.Init;
with Servlet.Core;
with Servlet.Server;
with Volume_Servlet;
with Util.Strings;
with Util.Log.Loggers;

procedure Volume_Server is
   Compute : aliased Volume_Servlet.Servlet;
   App     : aliased Servlet.Core.Servlet_Registry;
   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Volume_Server");
   Config  : Servlet.Server.Configuration;
   WS      : Server.Init.Container_Type;
begin
   Server.Configure (WS, Config);

   --  Register the servlets and filters
   App.Add_Servlet (Name => "compute", Server => Compute'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "compute", Pattern => "*.html");

   WS.Register_Application ("/volume", App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:{}/volume/compute.html",
             Util.Strings.Image (Config.Listening_Port));

   WS.Start;

   delay 60.0;

end Volume_Server;
