-----------------------------------------------------------------------
--  servlet-servlets-rest -- REST servlet
--  Copyright (C) 2016, 2017, 2018 Stephane Carrez
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

with Servlet.Streams.JSON;
package body Servlet.Servlets.Rest is

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Rest_Servlet;
                         Context : in Servlet_Registry'Class) is
      pragma Unreferenced (Context);
   begin
      null;
   end Initialize;

   --  ------------------------------
   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   --  ------------------------------
   overriding
   procedure Service (Server   : in Rest_Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      Method : constant String := Request.Get_Method;
   begin
      if Method = "GET" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.GET, Request, Response);

      elsif Method = "POST" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.POST, Request, Response);

      elsif Method = "PUT" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.PUT, Request, Response);

      elsif Method = "DELETE" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.DELETE, Request, Response);

      elsif Method = "HEAD" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.POST, Request, Response);

      elsif Method = "OPTIONS" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.HEAD, Request, Response);

      elsif Method = "TRACE" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.TRACE, Request, Response);

      elsif Method = "CONNECT" then
         Rest_Servlet'Class (Server).Dispatch (Servlet.Rest.CONNECT, Request, Response);

      else
         Response.Send_Error (Responses.SC_NOT_IMPLEMENTED);
      end if;
   end Service;

   procedure Dispatch (Server   : in Rest_Servlet;
                       Method   : in Servlet.Rest.Method_Type;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server);
      use type Servlet.Routes.Route_Type_Access;
      use type Servlet.Routes.Servlets.Rest.API_Route_Type;
      use type Servlet.Rest.Descriptor_Access;

      Route  : constant Servlet.Routes.Route_Type_Access := Request.Get_Route;
   begin
      if Route = null or else not (Route.all in Servlet.Routes.Servlets.Rest.API_Route_Type'Class) then
         Response.Set_Status (Servlet.Responses.SC_NOT_FOUND);
         return;
      end if;
      declare
         Api    : constant access Servlet.Routes.Servlets.Rest.API_Route_Type
           := Servlet.Routes.Servlets.Rest.API_Route_Type (Route.all)'Access;
         Desc   : constant Servlet.Rest.Descriptor_Access := Api.Descriptors (Method);
         Output : constant Servlet.Streams.Print_Stream := Response.Get_Output_Stream;
         Stream : Servlet.Streams.JSON.Print_Stream;
      begin
         if Desc = null then
            Response.Set_Status (Servlet.Responses.SC_NOT_FOUND);
            return;
         end if;
--         if not App.Has_Permission (Desc.Permission) then
--            Response.Set_Status (Servlet.Responses.SC_FORBIDDEN);
--            return;
--         end if;
         Servlet.Streams.JSON.Initialize (Stream, Output);
         Response.Set_Content_Type ("application/json; charset=utf-8");
         Api.Descriptors (Method).Dispatch (Request, Response, Stream);
      end;
   end Dispatch;

   function Create_Route (Registry : in Servlet.Servlets.Servlet_Registry;
                          Name     : in String)
                          return Servlet.Routes.Servlets.Rest.API_Route_Type_Access is
      Pos    : constant Servlet_Maps.Cursor := Registry.Servlets.Find (Name);
      Result : Servlet.Routes.Servlets.Rest.API_Route_Type_Access;
   begin
      if not Servlet_Maps.Has_Element (Pos) then
         --  Log.Error ("No servlet {0}", Name);
         raise Servlet_Error with "No servlet " & Name;
      end if;
      Result := new Servlet.Routes.Servlets.Rest.API_Route_Type;
      Result.Servlet := Servlet_Maps.Element (Pos);
      return Result;
   end Create_Route;

   --  Create a route for the REST API.
   function Create_Route (Servlet  : in Servlet.Servlets.Servlet_Access)
                          return Servlet.Routes.Servlets.Rest.API_Route_Type_Access is
      Result : Servlet.Routes.Servlets.Rest.API_Route_Type_Access;
   begin
      Result := new Servlet.Routes.Servlets.Rest.API_Route_Type;
      Result.Servlet := Servlet;
      return Result;
   end Create_Route;

end Servlet.Servlets.Rest;