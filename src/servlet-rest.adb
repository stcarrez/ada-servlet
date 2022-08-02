-----------------------------------------------------------------------
--  servlet-rest -- REST Support
--  Copyright (C) 2016, 2017, 2018, 2022 Stephane Carrez
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
with Util.Http.Headers;
with Servlet.Routes;
with Servlet.Routes.Servlets.Rest;
with Servlet.Core.Rest;
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

end Servlet.Rest;
