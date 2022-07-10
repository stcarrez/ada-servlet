-----------------------------------------------------------------------
--  volume_servlet -- Servlet example to compute some volumes
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
