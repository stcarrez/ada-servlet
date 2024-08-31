-----------------------------------------------------------------------
--  servlet-server-web-setup -- Generic method to give access to the internal HTTP server
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWS.Server;
generic
   with procedure Setup (Http : in out AWS.Server.HTTP);
procedure Servlet.Server.Web.Setup (Server : in out AWS_Container);
