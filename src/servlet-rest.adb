-----------------------------------------------------------------------
--  servlet-rest -- REST Support
--  Copyright (C) 2016, 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Http.Headers;
with Servlet.Routes;
with Servlet.Routes.Servlets.Rest;
with Servlet.Core.Rest;
with Servlet.Streams.Dynamic;
with EL.Contexts.Default;
with Util.Log.Loggers;
package body Servlet.Rest is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Servlet.Rest");

   --  ------------------------------
   --  Get the permission index associated with the REST operation.
   --  ------------------------------
   function Get_Permission (Handler : in Descriptor)
                            return Security.Permissions.Permission_Index is
   begin
      return Handler.Permission;
   end Get_Permission;

   --  ------------------------------
   --  Get the mime type selected for the operation.
   --  ------------------------------
   function Get_Mime_Type (Handler : in Descriptor;
                           Req     : in Servlet.Rest.Request'Class) return Mime_Access is
      Accept_Header : constant String := Req.Get_Header ("Accept");
   begin
      if Handler.Mimes /= null then
         return Util.Http.Headers.Get_Accepted (Accept_Header, Handler.Mimes.all);
      else
         return null;
      end if;
   end Get_Mime_Type;

   --  ------------------------------
   --  Get the stream type that should be configured before calling the operation handler.
   --  The choice is made on the operation capabilities defined by the Stream_Modes description.
   --  If there are multiple choices, the DYNAMIC stream type is prefered if it is enabled.
   --  Otherwise we look at the Accept header and decide according to the generated mime types.
   --  ------------------------------
   function Get_Stream_Type (Handler : in Descriptor;
                             Req     : in Servlet.Rest.Request'Class) return Stream_Type is
      use Streams.Dynamic;

      Best  : Stream_Type := Stream_Type'Last;
      Count : Natural := 0;
   begin
      for Kind in Stream_Type'Range loop
         if Handler.Streams (Kind) then
            Best := Kind;
            Count := Count + 1;
         end if;
      end loop;
      if Count = 1 then
         return Best;
      elsif Count = 0 or else Handler.Streams (DYNAMIC) then
         return DYNAMIC;
      else
         declare
            Mime : constant Mime_Access := Handler.Get_Mime_Type (Req);
         begin
            if Mime = null then
               if Handler.Streams (JSON) then
                  return JSON;
               elsif Handler.Streams (XML) then
                  return XML;
               else
                  return RAW;
               end if;
            elsif Mime.all = Util.Http.Mimes.Json then
               return JSON;
            elsif Mime.all = Util.Http.Mimes.Xml then
               return XML;
            else
               return RAW;
            end if;
         end;
      end if;
   end Get_Stream_Type;

   --  ------------------------------
   --  Register the API descriptor in a list.
   --  ------------------------------
   procedure Register (List : in out Descriptor_Access;
                       Item : in Descriptor_Access) is
   begin
      Item.Next := List;
      List := Item;
   end Register;

   --  ------------------------------
   --  Register the list of API descriptors for a given servlet and a root path.
   --  ------------------------------
   procedure Register (Registry  : in out Servlet.Core.Servlet_Registry;
                       Name      : in String;
                       URI       : in String;
                       ELContext : in EL.Contexts.ELContext'Class;
                       List      : in Descriptor_Access) is
      procedure Insert (Route : in out Servlet.Routes.Route_Type_Ref);
      Item : Descriptor_Access := List;

      procedure Insert (Route : in out Servlet.Routes.Route_Type_Ref) is
      begin
         if not Route.Is_Null then
            declare
               R : constant Servlet.Routes.Route_Type_Accessor := Route.Value;
               D : access Servlet.Routes.Servlets.Rest.API_Route_Type'Class;
            begin
               if not (R in Servlet.Routes.Servlets.Rest.API_Route_Type'Class) then
                  Log.Error ("Route API for {0}/{1} already used by another page",
                             URI, Item.Pattern.all);
                  return;
               end if;
               D := Servlet.Routes.Servlets.Rest.API_Route_Type'Class (R.Element.all)'Access;
               if D.Descriptors (Item.Method) /= null then
                  Log.Error ("Route API for {0}/{1} is already used", URI, Item.Pattern.all);
               end if;
               D.Descriptors (Item.Method) := Item;
            end;
         else
            declare
               D : access Servlet.Routes.Servlets.Rest.API_Route_Type'Class;
            begin
               D := Servlet.Core.Rest.Create_Route (Registry, Name);
               Route := Servlet.Routes.Route_Type_Refs.Create (D.all'Access);
               if D.Descriptors (Item.Method) /= null then
                  Log.Error ("Route API for {0}/{1} is already used", URI, Item.Pattern.all);
               end if;
               D.Descriptors (Item.Method) := Item;
            end;
         end if;
      end Insert;

   begin
      Log.Info ("Adding API route {0}", URI);
      while Item /= null loop
         Log.Debug ("Adding API route {0}/{1}", URI, Item.Pattern.all);
         Registry.Add_Route (URI & "/" & Item.Pattern.all, ELContext, Insert'Access);
         Item := Item.Next;
      end loop;
   end Register;

   --  ------------------------------
   --  Dispatch the request to the API handler.
   --  ------------------------------
   overriding
   procedure Dispatch (Handler : in Static_Descriptor;
                       Req     : in out Servlet.Rest.Request'Class;
                       Reply   : in out Servlet.Rest.Response'Class;
                       Stream  : in out Output_Stream'Class) is
   begin
      Handler.Handler (Req, Reply, Stream);
   end Dispatch;

   --  ------------------------------
   --  Register the API definition in the servlet registry.
   --  ------------------------------
   procedure Register (Registry   : in out Servlet.Core.Servlet_Registry'Class;
                       Definition : in Descriptor_Access) is
      use type Servlet.Core.Servlet_Access;
      procedure Insert (Route : in out Routes.Route_Type_Ref);

      Dispatcher : constant Servlet.Core.Request_Dispatcher
         := Registry.Get_Request_Dispatcher (Definition.Pattern.all);
      Servlet    : constant Core.Servlet_Access := Core.Get_Servlet (Dispatcher);

      procedure Insert (Route : in out Routes.Route_Type_Ref) is
      begin
         if not Route.Is_Null then
            declare
               R : constant Routes.Route_Type_Accessor := Route.Value;
               D : access Routes.Servlets.Rest.API_Route_Type'Class;
            begin
               if not (R in Routes.Servlets.Rest.API_Route_Type'Class) then
                  Log.Error ("Route API for {0} already used by another page",
                             Definition.Pattern.all);
                  D := Core.Rest.Create_Route (Servlet);
                  Route := Routes.Route_Type_Refs.Create (D.all'Access);
               else
                  D := Routes.Servlets.Rest.API_Route_Type'Class (R.Element.all)'Access;
               end if;
               if D.Descriptors (Definition.Method) /= null then
                  Log.Error ("Route API for {0} is already used", Definition.Pattern.all);
               end if;
               D.Descriptors (Definition.Method) := Definition;
            end;
         else
            declare
               D : access Routes.Servlets.Rest.API_Route_Type'Class;
            begin
               D := Core.Rest.Create_Route (Servlet);
               Route := Routes.Route_Type_Refs.Create (D.all'Access);
               if D.Descriptors (Definition.Method) /= null then
                  Log.Error ("Route API for {0} is already used", Definition.Pattern.all);
               end if;
               D.Descriptors (Definition.Method) := Definition;
            end;
         end if;
      end Insert;

      Ctx     : EL.Contexts.Default.Default_Context;
   begin
      if Servlet = null then
         Log.Error ("Cannot register REST operation {0}: no REST servlet",
                    Definition.Pattern.all);
         return;
      end if;
      Registry.Add_Route (Definition.Pattern.all, Ctx, Insert'Access);
   end Register;

   procedure Choose_Content_Type (Req    : in out Servlet.Rest.Request'Class;
                                  Reply  : in out Servlet.Rest.Response'Class;
                                  Stream : in out Servlet.Rest.Output_Stream'Class) is
   begin
      declare
         Accept_Header : constant String := Req.Get_Header ("Accept");
         Mime : Mime_Access;
      begin
         Mime := Util.Http.Headers.Get_Accepted (Accept_Header, (1 => Util.Http.Mimes.Json'Access));
         if Mime /= null then
            Set_Content_Type (Reply, Mime.all, Stream);
            return;
         end if;
         Mime := Util.Http.Headers.Get_Accepted (Accept_Header, (1 => Util.Http.Mimes.Xml'Access));
         if Mime /= null then
            Set_Content_Type (Reply, Mime.all, Stream);
            return;
         end if;
         Servlet.Streams.Dynamic.Print_Stream'Class (Stream).Set_Stream_Type (Servlet.Streams.Dynamic.RAW);
      end;
   end Choose_Content_Type;

   --  Set the response Content-Type header and configure the stream accordingly.
   procedure Set_Content_Type (Reply  : in out Servlet.Rest.Response'Class;
                               Mime   : in String;
                               Stream : in out Servlet.Rest.Output_Stream'Class) is
      use Servlet.Streams;
   begin
      Reply.Set_Content_Type (Mime);
      if not (Stream in Servlet.Streams.Dynamic.Print_Stream'Class) then
         return;
      end if;
      if Util.Http.Mimes.Is_Mime (Mime, Util.Http.Mimes.Json) then
         Dynamic.Print_Stream'Class (Stream).Set_Stream_Type (Dynamic.JSON);
         Stream.Start_Document;

      elsif Util.Http.Mimes.Is_Mime (Mime, Util.Http.Mimes.Xml) then
         Dynamic.Print_Stream'Class (Stream).Set_Stream_Type (Dynamic.XML);

      else
         Dynamic.Print_Stream'Class (Stream).Set_Stream_Type (Dynamic.RAW);
      end if;
   end Set_Content_Type;

end Servlet.Rest;
