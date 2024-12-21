-----------------------------------------------------------------------
--  upload_server -- Example of server with a servlet
--  Copyright (C) 2012, 2015, 2018, 2021, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Server.Init;
with Servlet.Core;
with Servlet.Server;
with Upload_Servlet;
with Util.Log.Loggers;
with Util.Strings;

procedure Upload_Server is
   Upload  : aliased Upload_Servlet.Servlet;
   App     : aliased Servlet.Core.Servlet_Registry;
   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Upload_Server");
   Config  : Servlet.Server.Configuration;
   WS      : Server.Init.Container_Type;
begin
   Config.Upload_Directory := Ada.Strings.Unbounded.To_Unbounded_String (".");
   Server.Configure (WS, Config);

   --  Register the servlets and filters
   App.Add_Servlet (Name => "upload", Server => Upload'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "upload", Pattern => "*.html");

   WS.Register_Application ("/upload", App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:{}/upload/upload.html",
             Util.Strings.Image (Config.Listening_Port));

   WS.Start;

   delay 6000.0;

end Upload_Server;
