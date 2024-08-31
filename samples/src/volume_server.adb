-----------------------------------------------------------------------
--  volume_server -- Example of server with a servlet
--  Copyright (C) 2010, 2015, 2018, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Server;
with Servlet.Core;
with Volume_Servlet;
with Util.Log.Loggers;

procedure Volume_Server is
   CONFIG_PATH  : constant String := "samples.properties";

   Compute : aliased Volume_Servlet.Servlet;
   App     : aliased Servlet.Core.Servlet_Registry;
   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Volume_Server");
begin
   Util.Log.Loggers.Initialize (CONFIG_PATH);

   --  Register the servlets and filters
   App.Add_Servlet (Name => "compute", Server => Compute'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "compute", Pattern => "*.html");

   Server.WS.Register_Application ("/volume", App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:8080/volume/compute.html");

   Server.WS.Start;

   delay 60.0;

end Volume_Server;
