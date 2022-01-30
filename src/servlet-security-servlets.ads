-----------------------------------------------------------------------
--  security-openid-servlets - Servlets for OpenID 2.0 Authentication
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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
with Servlet.Principals;

with Security.Auth; use Security;

--  The <b>Security.Openid.Servlets</b> package defines two servlets that can be used
--  to implement an OpenID 2.0 authentication with an OpenID provider such as Google,
--  Yahoo!, Verisign, Orange, ...
--
--  The process to use these servlets is the following:
--  <ul>
--    <li>Declare and register a <b>Request_Auth_Servlet</b> servlet.</li>
--    <li>Map that servlet to an authenticate URL.  To authenticate, a user will have
--        to click on a link mapped to that servlet.  To identify the OpenID provider,
--        the URL must contain the name of the provider.  Example:
--
--           http://localhost:8080/app/openid/auth/google
--
--    <li>Declare and register a <b>Verify_Auth_Servlet</b> servlet.
--    <li>Map that servlet to the verification URL.  This is the URL that the user
--        will return to when authentication is done by the OpenID provider.  Example:
--
--           http://localhost:8080/app/openid/verify
--
--    <li>Declare in the application properties a set of configurations:
--        <ul>
--            <li>The <b>openid.realm</b> must indicate the URL that identifies the
--                application the end user will trust.  Example:
--
--                    openid.realm=http://localhost:8080/app
--
--            <li>The <b>openid.callback_url</b> must indicate the callback URL to
--                which the OpenID provider will redirect the user when authentication
--                is finished (successfully or not).  The callback URL must match
--                the realm.
--
--                    openid.callback_url=http://localhost:8080/app/openid/verify
--
--            <li>For each OpenID provider, a URL to the Yadis entry point of the
--                OpenID provider is necessary.  This URL is used to get the XRDS
--                stream giving endpoint of the OpenID provider.  Example:
--
--                    openid.provider.google=https://www.google.com/accounts/o8/id
--        </ul>
--
--  </ul>
--
package Servlet.Security.Servlets is

   --  ------------------------------
   --  OpenID Servlet
   --  ------------------------------
   --  The <b>Openid_Servlet</b> is the OpenID root servlet for OpenID 2.0 authentication.
   --  It is defined to provide a common basis for the authentication and verification servlets.
   type Openid_Servlet is abstract new Servlet.Core.Servlet and Auth.Parameters with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Openid_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class);

   --  Get a configuration parameter from the servlet context for the security Auth provider.
   overriding
   function Get_Parameter (Server : in Openid_Servlet;
                           Name   : in String) return String;

   --  ------------------------------
   --  OpenID Request Servlet
   --  ------------------------------
   --  The <b>Request_Auth_Servlet</b> servlet implements the first steps of an OpenID
   --  authentication.
   type Request_Auth_Servlet is new Openid_Servlet with private;

   --  Proceed to the OpenID authentication with an OpenID provider.
   --  Find the OpenID provider URL and starts the discovery, association phases
   --  during which a private key is obtained from the OpenID provider.
   --  After OpenID discovery and association, the user will be redirected to
   --  the OpenID provider.
   overriding
   procedure Do_Get (Server   : in Request_Auth_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class);

   --  ------------------------------
   --  OpenID Verification Servlet
   --  ------------------------------
   --  The <b>Verify_Auth_Servlet</b> verifies the authentication result and
   --  extract authentication from the callback URL.
   type Verify_Auth_Servlet is new Openid_Servlet with private;

   --  Verify the authentication result that was returned by the OpenID provider.
   --  If the authentication succeeded and the signature was correct, sets a
   --  user principals on the session.
   overriding
   procedure Do_Get (Server   : in Verify_Auth_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class);

   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Credential</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   procedure Create_Principal (Server     : in Verify_Auth_Servlet;
                               Credential : in Auth.Authentication;
                               Result     : out Servlet.Principals.Principal_Access);

private
   function Get_Provider_URL (Server   : in Request_Auth_Servlet;
                              Request  : in Servlet.Requests.Request'Class) return String;

   procedure Initialize (Server   : in Openid_Servlet;
                         Provider : in String;
                         Manager  : in out Auth.Manager);

   type Openid_Servlet is new Servlet.Core.Servlet and Auth.Parameters with null record;

   type Request_Auth_Servlet is new Openid_Servlet with null record;
   type Verify_Auth_Servlet is new Openid_Servlet with null record;

end Servlet.Security.Servlets;
