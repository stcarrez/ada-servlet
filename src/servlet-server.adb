-----------------------------------------------------------------------
--  servlet-server -- Servlet Server
--  Copyright (C) 2009, 2010, 2011, 2015, 2016, 2018, 2019, 2020 Stephane Carrez
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

with Util.Strings;
with Util.Log.Loggers;

with Ada.Unchecked_Deallocation;
with Ada.Task_Attributes;
package body Servlet.Server is

   use type Core.Status_Type;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Servlet.Server");

   Null_Context : constant Request_Context := Request_Context'(null, null, null);

   package Task_Context is new Ada.Task_Attributes
     (Request_Context, Null_Context);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Binding_Array,
                                     Name   => Binding_Array_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Binding,
                                     Name   => Binding_Access);

   --  ------------------------------
   --  Get the current registry associated with the current request being processed
   --  by the current thread.  Returns null if there is no current request.
   --  ------------------------------
   function Current return Servlet.Core.Servlet_Registry_Access is
   begin
      return Task_Context.Value.Application;
   end Current;

   --  ------------------------------
   --  Set the current registry (for unit testing mostly).
   --  ------------------------------
   procedure Set_Context (Context : in Servlet.Core.Servlet_Registry_Access) is
      C : Request_Context;
   begin
      C.Application := Context;
      C.Request     := null;
      C.Response    := null;
      Set_Context (C);
   end Set_Context;

   --  ------------------------------
   --  Set the current registry.  This is called by <b>Service</b> once the
   --  registry is identified from the URI.
   --  ------------------------------
   procedure Set_Context (Context : in Request_Context) is
   begin
      Task_Context.Set_Value (Context);
   end Set_Context;

   --  ------------------------------
   --  Give access to the current request and response object to the `Process`
   --  procedure.  If there is no current request for the thread, do nothing.
   --  ------------------------------
   procedure Update_Context (Process : not null access
                               procedure (Request  : in out Requests.Request'Class;
                                          Response : in out Responses.Response'Class)) is
      Ctx : constant Request_Context := Task_Context.Value;
   begin
      Process (Ctx.Request.all, Ctx.Response.all);
   end Update_Context;

   --  ------------------------------
   --  Register the application to serve requests
   --  ------------------------------
   procedure Register_Application (Server  : in out Container;
                                   URI     : in String;
                                   Context : in Core.Servlet_Registry_Access) is
      Count : constant Natural := Server.Nb_Bindings;
      Apps  : constant Binding_Array_Access := new Binding_Array (1 .. Count + 1);
      Old   : Binding_Array_Access := Server.Applications;
   begin
      Log.Info ("Register application {0}", URI);

      if Old /= null then
         Apps (1 .. Count) := Server.Applications (1 .. Count);
      end if;
      Apps (Count + 1) := new Binding '(Len => URI'Length, Context => Context, Base_URI => URI);

      --  Inform the servlet registry about the base URI.
      Context.Register_Application (URI);

      --  Start the application if the container is started.
      if Server.Is_Started and then Context.Get_Status = Core.Ready then
         Context.Start;
      end if;

      --  Update the binding.
      Server.Applications := Apps;
      Server.Nb_Bindings := Count + 1;
      if Old /= null then
         Free (Old);
      end if;
   end Register_Application;

   --  ------------------------------
   --  Remove the application
   --  ------------------------------
   procedure Remove_Application (Server  : in out Container;
                                 Context : in Core.Servlet_Registry_Access) is
      use type Servlet.Core.Servlet_Registry_Access;

      Count : constant Natural := Server.Nb_Bindings;
      Old   : Binding_Array_Access := Server.Applications;
      Apps  : Binding_Array_Access;
   begin
      for I in 1 .. Count loop
         if Old (I).Context = Context then
            Log.Info ("Removed application {0}", Old (I).Base_URI);
            Free (Old (I));
            if I < Count then
               Old (I) := Old (Count);
            end if;
            if Count > 1 then
               Apps := new Binding_Array (1 .. Count - 1);
               Apps.all := Old (1 .. Count - 1);
            else
               Apps := null;
            end if;
            Server.Applications := Apps;
            Server.Nb_Bindings := Count - 1;
            Free (Old);
            return;
         end if;
      end loop;
   end Remove_Application;

   --  ------------------------------
   --  Start the applications that have been registered.
   --  ------------------------------
   procedure Start (Server : in out Container) is
   begin
      if Server.Applications /= null then
         for Application of Server.Applications.all loop
            if Application.Context.Get_Status = Core.Ready then
               Log.Info ("Starting application {0}", Application.Base_URI);
               Application.Context.Start;
            end if;
         end loop;
      end if;
      Server.Is_Started := True;
   end Start;

   --  ------------------------------
   --  Receives standard HTTP requests from the public service method and
   --  dispatches them to the Do_XXX methods defined in this class. This method
   --  is an HTTP-specific version of the Servlet.service(Request, Response)
   --  method. There's no need to override this method.
   --  ------------------------------
   procedure Service (Server   : in Container;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is

      use Util.Strings;

      URI        : constant String := Request.Get_Request_URI;
      Slash_Pos  : constant Natural := Index (URI, '/', URI'First + 1);
      Apps       : constant Binding_Array_Access := Server.Applications;
      Prefix_End : Natural;
   begin
      if Apps = null then
         Response.Set_Status (Responses.SC_NOT_FOUND);
         Server.Default.Send_Error_Page (Request, Response);
         return;
      end if;

      --  Find the module and action to invoke
      if Slash_Pos > 1 then
         Prefix_End := Slash_Pos - 1;
      else
         Prefix_End := URI'Last;
      end if;

      for Application of Apps.all loop
         if Application.Base_URI = URI (URI'First .. Prefix_End)
           and then Application.Context.Get_Status = Core.Started
         then
            declare
               Req        : Request_Context;
               Context    : constant Core.Servlet_Registry_Access := Application.Context;
               Page       : constant String := URI (Prefix_End + 1 .. URI'Last);
               Dispatcher : constant Core.Request_Dispatcher
                 := Context.Get_Request_Dispatcher (Page);
            begin
               Log.Info ("{0} {1}", Request.Get_Method, Page);
               Req.Request     := Request'Unchecked_Access;
               Req.Response    := Response'Unchecked_Access;
               Req.Application := Context;
               Set_Context (Req);
               Core.Forward (Dispatcher, Request, Response);
               case Response.Get_Status / 100 is
                  when 2 | 3 =>
                     null;

                  when others =>
                     if not Response.Is_Committed then
                        Context.Send_Error_Page (Request, Response);
                     end if;

               end case;
               Set_Context (Null_Context);
               return;

            exception
               when E : others =>
                  Context.Error (Request, Response, E);
                  Set_Context (Null_Context);
                  return;
            end;
         elsif Application.Len = 0 and then Application.Context.Get_Status = Core.Started then
            declare
               Req        : Request_Context;
               Context    : constant Core.Servlet_Registry_Access := Application.Context;
               Dispatcher : constant Core.Request_Dispatcher
                 := Context.Get_Request_Dispatcher (URI);
            begin
               Log.Info ("{0} {1}", Request.Get_Method, URI);
               Req.Request     := Request'Unchecked_Access;
               Req.Response    := Response'Unchecked_Access;
               Req.Application := Context;
               Set_Context (Req);
               Core.Forward (Dispatcher, Request, Response);
               case Response.Get_Status / 100 is
                  when 2 | 3 =>
                     null;

                  when others =>
                     if not Response.Is_Committed then
                        Context.Send_Error_Page (Request, Response);
                     end if;

               end case;
               Set_Context (Null_Context);
               return;

            exception
               when E : others =>
                  Context.Error (Request, Response, E);
                  Set_Context (Null_Context);
                  return;
            end;

         end if;
      end loop;

      Response.Set_Status (Responses.SC_NOT_FOUND);
      Server.Default.Send_Error_Page (Request, Response);

   exception
      when E : others =>
         Server.Default.Error (Request, Response, E);
   end Service;

   --  ------------------------------
   --  Iterate over the application which are registered.
   --  ------------------------------
   procedure Iterate (Server  : in Container;
                      Process : not null access
                        procedure (URI     : in String;
                                   Context : in Core.Servlet_Registry_Access)) is
   begin
      if Server.Applications /= null then
         for Application of Server.Applications.all loop
            Process (Application.Base_URI, Application.Context);
         end loop;
      end if;
   end Iterate;

   --  ------------------------------
   --  Release the storage.
   --  ------------------------------
   overriding
   procedure Finalize (Server : in out Container) is
   begin
      if Server.Applications /= null then
         for I in Server.Applications'Range loop
            Free (Server.Applications (I));
         end loop;
      end if;
      Free (Server.Applications);
   end Finalize;

end Servlet.Server;
