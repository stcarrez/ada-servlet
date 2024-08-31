-----------------------------------------------------------------------
--  servlet-security-oauth - OAuth2 servlets
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Core;
with Servlet.Requests;
with Servlet.Responses;

with Security.OAuth.Servers; use Security.OAuth;

package Servlet.Security.OAuth is

   --  The token servlet that grants an access token and refresh token.
   type Token_Servlet is new Servlet.Core.Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Token_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class);

   --  Set the authorization manager.
   procedure Set_Auth_Manager (Server  : in out Token_Servlet;
                               Manager : in Servers.Auth_Manager_Access);

   --  Perform the OAuth token request.  This is the last step in OAuth flow that allows
   --  to retrieve an access token and an optional refresh token.  This token request is
   --  used by the following OAuth methods:
   --  RFC 6749: 4.1.3.  Access Token Request
   --  RFC 6749: 4.3.  Resource Owner Password Credentials Grant
   --  RFC 6749: 4.4.  Client Credentials Grant
   --  RFC 6749: 4.5.  Extension Grants
   overriding
   procedure Do_Post (Server   : in Token_Servlet;
                      Request  : in out Servlet.Requests.Request'Class;
                      Response : in out Servlet.Responses.Response'Class);

private

   type Token_Servlet is new Servlet.Core.Servlet with record
      Manager : Servers.Auth_Manager_Access;
   end record;

end Servlet.Security.OAuth;
