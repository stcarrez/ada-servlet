-----------------------------------------------------------------------
--  servlet-security-oauth - OAuth2 servlets
--  Copyright (C) 2018 Stephane Carrez
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
with Servlet.Streams.JSON;
with Security.Auth; use Security;
package body Servlet.Security.OAuth is

   use Ada.Strings.Unbounded;
   use type Servers.Grant_Status;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Token_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class) is
   begin
      null;
   end Initialize;

   --  ------------------------------
   --  Set the authorization manager.
   --  ------------------------------
   procedure Set_Auth_Manager (Server  : in out Token_Servlet;
                               Manager : in Servers.Auth_Manager_Access) is
   begin
      Server.Manager := Manager;
   end Set_Auth_Manager;

   --  ------------------------------
   --  Perform the OAuth token request.  This is the last step in OAuth flow that allows
   --  to retrieve an access token and an optional refresh token.  This token request is
   --  used by the following OAuth methods:
   --  RFC 6749: 4.1.3.  Access Token Request
   --  RFC 6749: 4.3.  Resource Owner Password Credentials Grant
   --  RFC 6749: 4.4.  Client Credentials Grant
   --  RFC 6749: 4.5.  Extension Grants
   --  ------------------------------
   overriding
   procedure Do_Post (Server   : in Token_Servlet;
                      Request  : in out Servlet.Requests.Request'Class;
                      Response : in out Servlet.Responses.Response'Class) is
      type Auth_Params is new Auth.Parameters with null record;

      overriding
      function Get_Parameter (Params : in Auth_Params;
                              Name   : in String) return String;

      overriding
      function Get_Parameter (Params : in Auth_Params;
                              Name   : in String) return String is
         pragma Unreferenced (Params);
      begin
         return Request.Get_Parameter (Name);
      end Get_Parameter;

      Params : Auth_Params;
      Grant  : Servers.Grant_Type;
      Output : constant Streams.Print_Stream := Response.Get_Output_Stream;
      Stream : Streams.JSON.Print_Stream;

   begin
      Streams.JSON.Initialize (Stream, Output);
      Response.Set_Content_Type ("application/json; charset=utf-8");
      Server.Manager.Token (Params, Grant);

      if Grant.Status = Servers.Valid_Grant then
         Response.Set_Status (Servlet.Responses.SC_OK);
         Stream.Start_Document;
         Stream.Write_Entity ("access_token", To_String (Grant.Token));
         Stream.Write_Entity ("token_type", "Bearer");
         Stream.Write_Long_Entity ("expires_in", Long_Long_Integer (Grant.Expires_In));
         Stream.End_Document;
      else
         Response.Set_Status (Servlet.Responses.SC_BAD_REQUEST);
         Stream.Start_Document;
         Stream.Write_Entity ("error", Grant.Error.all);
         Stream.End_Document;
      end if;
   end Do_Post;

end Servlet.Security.OAuth;
