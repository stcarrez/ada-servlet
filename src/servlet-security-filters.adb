-----------------------------------------------------------------------
--  security-filters -- Security filter
--  Copyright (C) 2011, 2012, 2013, 2015, 2018, 2022 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Log.Loggers;

with Servlet.Cookies;

with Security.Contexts;
with Security.Policies.URLs;
package body Servlet.Security.Filters is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Security.Filters");

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config) is
      pragma Unreferenced (Server, Config);
   begin
      null;
   end Initialize;

   --  ------------------------------
   --  Set the permission manager that must be used to verify the permission.
   --  ------------------------------
   procedure Set_Permission_Manager (Filter  : in out Auth_Filter;
                                     Manager : in Policies.Policy_Manager_Access) is
   begin
      Filter.Manager := Manager;
   end Set_Permission_Manager;

   --  ------------------------------
   --  Filter the request to make sure the user is authenticated.
   --  Invokes the <b>Do_Login</b> procedure if there is no user.
   --  If a permission manager is defined, check that the user has the permission
   --  to view the page.  Invokes the <b>Do_Deny</b> procedure if the permission
   --  is denied.
   --  ------------------------------
   overriding
   procedure Do_Filter (F        : in Auth_Filter;
                        Request  : in out Servlet.Requests.Request'Class;
                        Response : in out Servlet.Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain) is
      use Ada.Strings.Unbounded;
      use Policies.URLs;
      use type Policies.Policy_Manager_Access;

      Session : Servlet.Sessions.Session;
      SID     : Unbounded_String;
      AID     : Unbounded_String;
      Auth    : Servlet.Principals.Principal_Access;

      pragma Unreferenced (SID);

      procedure Fetch_Cookie (Cookie : in Servlet.Cookies.Cookie);

      --  ------------------------------
      --  Collect the AID and SID cookies.
      --  ------------------------------
      procedure Fetch_Cookie (Cookie : in Servlet.Cookies.Cookie) is
         Name : constant String := Servlet.Cookies.Get_Name (Cookie);
      begin
         if Name = SID_COOKIE then
            SID := To_Unbounded_String (Servlet.Cookies.Get_Value (Cookie));
         elsif Name = AID_COOKIE then
            AID := To_Unbounded_String (Servlet.Cookies.Get_Value (Cookie));
         end if;
      end Fetch_Cookie;

      Context : aliased Contexts.Security_Context;
   begin
      Request.Iterate_Cookies (Fetch_Cookie'Access);

      --  Get a session but avoid creating it.
      Session := Request.Get_Session (Create => False);
      if Session.Is_Valid then
         Auth := Session.Get_Principal;
      end if;

      --  If the session does not have a principal, try to authenticate the user with
      --  the auto-login cookie.
      if Auth = null and then Length (AID) > 0 then
         Auth_Filter'Class (F).Authenticate (Request, Response, Session, To_String (AID), Auth);
         if Auth /= null then

            --  Now we must make sure we have a valid session and create it if necessary.
            if not Session.Is_Valid then
               Session := Request.Get_Session (Create => True);
            end if;
            Session.Set_Principal (Auth);
         end if;
      end if;

      --  A permission manager is installed, check that the user can display the page.
      if F.Manager /= null then
         if Auth = null then
            Context.Set_Context (F.Manager, null);
         else
            Context.Set_Context (F.Manager, Auth.all'Access);
         end if;
         declare
            Servlet : constant String := Request.Get_Servlet_Path;
            URL     : constant String := Servlet & Request.Get_Path_Info;
            Perm    : constant Policies.URLs.URL_Permission (URL'Length)
              := URL_Permission '(Len => URL'Length, URL => URL);
         begin
            if not F.Manager.Has_Permission (Context, Perm) then
               if Auth = null then
                  --  No permission and no principal, redirect to the login page.
                  Log.Info ("Page need authentication on {0}", URL);
                  Auth_Filter'Class (F).Do_Login (Request, Response);
               else
                  Log.Info ("Deny access on {0}", URL);
                  Auth_Filter'Class (F).Do_Deny (Request, Response);
               end if;
               return;
            end if;
            Log.Debug ("Access granted on {0}", URL);
         end;
      end if;

      --  Request is authorized, proceed to the next filter.
      Servlet.Core.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);
   end Do_Filter;

   --  ------------------------------
   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   --  ------------------------------
   procedure Do_Login (F        : in Auth_Filter;
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class) is
      pragma Unreferenced (F, Request);
   begin
      Response.Send_Error (Servlet.Responses.SC_UNAUTHORIZED);
   end Do_Login;

   --  ------------------------------
   --  Display the forbidden access page.  This procedure is called when the user is not
   --  authorized to see the page.  The default implementation returns the SC_FORBIDDEN error.
   --  ------------------------------
   procedure Do_Deny (F        : in Auth_Filter;
                      Request  : in out Servlet.Requests.Request'Class;
                      Response : in out Servlet.Responses.Response'Class) is
      pragma Unreferenced (F, Request);
   begin
      Response.Set_Status (Servlet.Responses.SC_FORBIDDEN);
   end Do_Deny;

   --  ------------------------------
   --  Authenticate a user by using the auto-login cookie.  This procedure is called if the
   --  current session does not have any principal.  Based on the request and the optional
   --  auto-login cookie passed in <b>Auth_Id</b>, it should identify the user and return
   --  a principal object.  The principal object will be freed when the session is closed.
   --  If the user cannot be authenticated, the returned principal should be null.
   --
   --  The default implementation returns a null principal.
   --  ------------------------------
   procedure Authenticate (F        : in Auth_Filter;
                           Request  : in out Servlet.Requests.Request'Class;
                           Response : in out Servlet.Responses.Response'Class;
                           Session  : in Servlet.Sessions.Session;
                           Auth_Id  : in String;
                           Principal : out Servlet.Principals.Principal_Access) is
      pragma Unreferenced (F, Request, Response, Session, Auth_Id);
   begin
      Principal := null;
   end Authenticate;

end Servlet.Security.Filters;
