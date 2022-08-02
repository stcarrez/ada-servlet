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
with Ada.Strings.Unbounded;

with Servlet.Filters;
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Core;
with Security.OAuth.Servers; use Security.OAuth;

--  The <b>Servlet.Security.Filters.OAuth</b> package provides a servlet filter that
--  implements the RFC 6749 "Accessing Protected Resources" part: it extracts the OAuth
--  access token, verifies the grant and the permission.  The servlet filter implements
--  the RFC 6750 "OAuth 2.0 Bearer Token Usage".
--
package Servlet.Security.Filters.OAuth is

   --  RFC 2617 HTTP header for authorization.
   AUTHORIZATION_HEADER_NAME    : constant String := "Authorization";

   --  RFC 2617 HTTP header for failed authorization.
   WWW_AUTHENTICATE_HEADER_NAME : constant String := "WWW-Authenticate";

   type Auth_Filter is new Servlet.Filters.Filter with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config);

   --  Set the authorization manager that must be used to verify the OAuth token.
   procedure Set_Auth_Manager (Filter  : in out Auth_Filter;
                               Manager : in Servers.Auth_Manager_Access);

   --  Filter the request to make sure the user is authenticated.
   --  Invokes the <b>Do_Login</b> procedure if there is no user.
   --  If a permission manager is defined, check that the user has the permission
   --  to view the page.  Invokes the <b>Do_Deny</b> procedure if the permission
   --  is denied.
   overriding
   procedure Do_Filter (F        : in Auth_Filter;
                        Request  : in out Servlet.Requests.Request'Class;
                        Response : in out Servlet.Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain);

   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   procedure Do_Login (F        : in Auth_Filter;
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class);

   --  Display the forbidden access page.  This procedure is called when the user is not
   --  authorized to see the page.  The default implementation returns the SC_FORBIDDEN error.
   procedure Do_Deny (F        : in Auth_Filter;
                      Request  : in out Servlet.Requests.Request'Class;
                      Response : in out Servlet.Responses.Response'Class);

private

   type Auth_Filter is new Servlet.Filters.Filter with record
      Realm     : Servers.Auth_Manager_Access;
      Realm_URL : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Servlet.Security.Filters.OAuth;
