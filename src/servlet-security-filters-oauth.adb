-----------------------------------------------------------------------
--  servlet-security-filters-oauth -- OAuth Security filter
--  Copyright (C) 2017, 2018, 2022 Stephane Carrez
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
with Ada.Strings.Fixed;
with Util.Log.Loggers;

package body Servlet.Security.Filters.OAuth is

   use Ada.Strings.Unbounded;
   use Servers;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Security.Filters.OAuth");

   function Get_Access_Token (Request : in Servlet.Requests.Request'Class) return String;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config) is
   begin
      null;
   end Initialize;

   --  ------------------------------
   --  Set the authorization manager that must be used to verify the OAuth token.
   --  ------------------------------
   procedure Set_Auth_Manager (Filter  : in out Auth_Filter;
                               Manager : in Servers.Auth_Manager_Access) is
   begin
      Filter.Realm := Manager;
   end Set_Auth_Manager;

   function Get_Access_Token (Request : in Servlet.Requests.Request'Class) return String is
      Header : constant String := Request.Get_Header (AUTHORIZATION_HEADER_NAME);
   begin
      if Header'Length < 7 or else Header (Header'First .. Header'First + 6) /= "Bearer " then
         return "";
      end if;

      return Ada.Strings.Fixed.Trim (Header (Header'First + 7 .. Header'Last), Ada.Strings.Both);
   end Get_Access_Token;

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
      Servlet : constant String := Request.Get_Servlet_Path;
      URL     : constant String := Servlet & Request.Get_Path_Info;
   begin
      if F.Realm = null then
         Log.Error ("Deny access on {0} due to missing realm", URL);
         Auth_Filter'Class (F).Do_Deny (Request, Response);
         return;
      end if;
      declare
         Bearer  : constant String := Get_Access_Token (Request);
         --  Auth    : Principal_Access;
         Grant   : Servers.Grant_Type;
      begin
         if Bearer'Length = 0 then
            Log.Info ("Ask authentication on {0} due to missing access token", URL);
--              Auth_Filter'Class (F).Do_Login (Request, Response);
            Core.Do_Filter (Chain    => Chain,
                            Request  => Request,
                            Response => Response);
            return;
         end if;
         F.Realm.Authenticate (Bearer, Grant);
         if Grant.Status /= Valid_Grant then
            Log.Info ("Ask authentication on {0} due to missing access token", URL);
            Auth_Filter'Class (F).Do_Deny (Request, Response);
            return;
         end if;

         Request.Set_User_Principal (Grant.Auth.all'Access);
         Core.Do_Filter (Chain    => Chain,
                         Request  => Request,
                         Response => Response);
      end;
   end Do_Filter;

   --  ------------------------------
   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   --  ------------------------------
   procedure Do_Login (F        : in Auth_Filter;
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class) is
      pragma Unreferenced (Request);
   begin
      Response.Send_Error (Servlet.Responses.SC_UNAUTHORIZED);
      Response.Add_Header (WWW_AUTHENTICATE_HEADER_NAME,
                           "Bearer realm=""" & To_String (F.Realm_URL)
                           & """, error=""invalid_token""");
   end Do_Login;

   --  ------------------------------
   --  Display the forbidden access page.  This procedure is called when the user is not
   --  authorized to see the page.  The default implementation returns the SC_FORBIDDEN error.
   --  ------------------------------
   procedure Do_Deny (F        : in Auth_Filter;
                      Request  : in out Servlet.Requests.Request'Class;
                      Response : in out Servlet.Responses.Response'Class) is
      pragma Unreferenced (Request);
   begin
      Response.Add_Header (WWW_AUTHENTICATE_HEADER_NAME,
                           "Bearer realm=""" & To_String (F.Realm_URL)
                           & """, error=""invalid_token""");
      Response.Set_Status (Servlet.Responses.SC_FORBIDDEN);
   end Do_Deny;

end Servlet.Security.Filters.OAuth;
