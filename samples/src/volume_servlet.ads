-----------------------------------------------------------------------
--  volume_servlet -- Servlet example to compute some volumes
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Core;
with Servlet.Requests;
with Servlet.Responses;

package Volume_Servlet is

   use Servlet;

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Servlet is new Core.Servlet with null record;

   --  Called by the servlet container when a GET request is received.
   --  Display the volume form page.
   overriding
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Called by the servlet container when a POST request is received.
   --  Computes the cylinder volume and display the result page.
   overriding
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

end Volume_Servlet;
