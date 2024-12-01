-----------------------------------------------------------------------
--  servlet-servlets-rest -- REST servlet
--  Copyright (C) 2016, 2017, 2018, 2019, 2020, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Streams.JSON;
with Servlet.Streams.XML;
with Servlet.Streams.Raw;
with Servlet.Streams.Dynamic;
package body Servlet.Core.Rest is

   use type Util.Strings.Name_Access;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   overriding
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
         Rest_Servlet'Class (Server).Dispatch (GET, Request, Response);

      elsif Method = "POST" then
         Rest_Servlet'Class (Server).Dispatch (POST, Request, Response);

      elsif Method = "PUT" then
         Rest_Servlet'Class (Server).Dispatch (PUT, Request, Response);

      elsif Method = "DELETE" then
         Rest_Servlet'Class (Server).Dispatch (DELETE, Request, Response);

      elsif Method = "HEAD" then
         Rest_Servlet'Class (Server).Dispatch (HEAD, Request, Response);

      elsif Method = "OPTIONS" then
         Rest_Servlet'Class (Server).Dispatch (OPTIONS, Request, Response);

      elsif Method = "TRACE" then
         Rest_Servlet'Class (Server).Dispatch (TRACE, Request, Response);

      elsif Method = "PATCH" then
         Rest_Servlet'Class (Server).Dispatch (PATCH, Request, Response);

      elsif Method = "CONNECT" then
         Rest_Servlet'Class (Server).Dispatch (CONNECT, Request, Response);

      else
         Response.Send_Error (Responses.SC_NOT_IMPLEMENTED);
      end if;
   end Service;

   procedure Dispatch (Server   : in Rest_Servlet;
                       Method   : in Method_Type;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server);

   begin
      if not Request.Has_Route then
         Response.Set_Status (Responses.SC_NOT_FOUND);
         Response.Set_Committed;
         return;
      end if;
      declare
         Route  : constant Routes.Route_Type_Accessor := Request.Get_Route;
      begin
         if not (Route in Routes.Servlets.Rest.API_Route_Type'Class) then
            Response.Set_Status (Responses.SC_NOT_FOUND);
            Response.Set_Committed;
            return;
         end if;
         declare
            use Streams.Dynamic;

            Api    : constant access Routes.Servlets.Rest.API_Route_Type'Class
              := Routes.Servlets.Rest.API_Route_Type'Class (Route.Element.all)'Access;
            Desc   : constant Descriptor_Access := Api.Descriptors (Method);
            Kind   : Streams.Dynamic.Stream_Type;
         begin
            if Desc = null then
               Response.Set_Status (Responses.SC_NOT_FOUND);
               Response.Set_Committed;
               return;
            end if;
            Kind := Desc.Get_Stream_Type (Request);
            case Kind is
               when Streams.Dynamic.JSON =>
                  declare
                     Stream : Streams.JSON.Print_Stream := Response.Get_Output_Stream;
                  begin
                     Response.Set_Content_Type ("application/json; charset=utf-8");
                     Api.Descriptors (Method).Dispatch (Request, Response, Stream);
                  end;

               when XML =>
                  declare
                     Stream : Streams.XML.Print_Stream := Response.Get_Output_Stream;
                  begin
                     Response.Set_Content_Type ("application/xml; charset=utf-8");
                     Api.Descriptors (Method).Dispatch (Request, Response, Stream);
                  end;

               when RAW | FORM =>
                  declare
                     Output : constant Streams.Print_Stream := Response.Get_Output_Stream;
                     Stream : Streams.Raw.Print_Stream;
                     Mime   : constant Mime_Access := Desc.Get_Mime_Type (Request);
                  begin
                     Streams.Raw.Initialize (Stream, Output);

                     if Mime /= null then
                        Response.Set_Content_Type (Mime.all);
                     end if;
                     Api.Descriptors (Method).Dispatch (Request, Response, Stream);
                  end;

               when DYNAMIC =>
                  declare
                     Stream : Streams.Dynamic.Print_Stream := Response.Get_Output_Stream;
                     Mime   : constant Mime_Access := Desc.Get_Mime_Type (Request);
                  begin
                     if Mime /= null then
                        Response.Set_Content_Type (Mime.all);
                     end if;
                     Api.Descriptors (Method).Dispatch (Request, Response, Stream);
                  end;

            end case;
         end;
      end;
   end Dispatch;

   function Create_Route (Registry : in Core.Servlet_Registry;
                          Name     : in String)
                          return Routes.Servlets.Rest.API_Route_Type_Access is
      Pos    : constant Servlet_Maps.Cursor := Registry.Servlets.Find (Name);
      Result : Routes.Servlets.Rest.API_Route_Type_Access;
   begin
      if not Servlet_Maps.Has_Element (Pos) then
         --  Log.Error ("No servlet {0}", Name);
         raise Servlet_Error with "No servlet " & Name;
      end if;
      Result := new Routes.Servlets.Rest.API_Route_Type;
      Result.Servlet := Servlet_Maps.Element (Pos);
      return Result;
   end Create_Route;

   --  Create a route for the REST API.
   function Create_Route (Servlet  : in Core.Servlet_Access)
                          return Routes.Servlets.Rest.API_Route_Type_Access is
      Result : Routes.Servlets.Rest.API_Route_Type_Access;
   begin
      Result := new Routes.Servlets.Rest.API_Route_Type;
      Result.Servlet := Servlet;
      return Result;
   end Create_Route;

end Servlet.Core.Rest;
