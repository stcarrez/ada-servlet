-----------------------------------------------------------------------
--  servlet-servlets-rest -- REST servlet
--  Copyright (C) 2016, 2017, 2018, 2019 Stephane Carrez
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
with Servlet.Rest;
with Servlet.Routes.Servlets.Rest;

use Servlet.Rest;
package Servlet.Core.Rest is

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Rest_Servlet is new Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Rest_Servlet;
                         Context : in Servlet_Registry'Class);

   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   overriding
   procedure Service (Server   : in Rest_Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Create a route for the REST API.
   function Create_Route (Registry : in Servlet_Registry;
                          Name     : in String)
                          return Routes.Servlets.Rest.API_Route_Type_Access;
   function Create_Route (Servlet  : in Servlet_Access)
                          return Routes.Servlets.Rest.API_Route_Type_Access;

   procedure Dispatch (Server   : in Rest_Servlet;
                       Method   : in Method_Type;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class);

private

   type Rest_Servlet is new Servlet with null record;

end Servlet.Core.Rest;
