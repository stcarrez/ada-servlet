-----------------------------------------------------------------------
--  servlet-server-web-setup -- Generic method to give access to the internal HTTP server
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

procedure Servlet.Server.Web.Setup (Server : in out AWS_Container) is
begin
   Setup (Server.WS);
end Servlet.Server.Web.Setup;
