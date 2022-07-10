-----------------------------------------------------------------------
--  servlet-requests.mockup -- Servlet Requests mockup
--  Copyright (C) 2010, 2011, 2012, 2013, 2017, 2020, 2021, 2022 Stephane Carrez
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
package body Servlet.Requests.Mockup is

   function Find (Map  : in Util.Strings.Maps.Map;
                  Name : in String) return String;

   --  ------------------------------
   --  Find and return the string associated with a key in the map.
   --  ------------------------------
   function Find (Map  : in Util.Strings.Maps.Map;
                  Name : in String) return String is
      Pos : constant Util.Strings.Maps.Cursor := Map.Find (Name);
   begin
      if Util.Strings.Maps.Has_Element (Pos) then
         return Util.Strings.Maps.Element (Pos);
      else
         return "";
      end if;
   end Find;

   --  ------------------------------
   --  Returns the value of a request parameter as a String, or null if the
   --  parameter does not exist. Request parameters are extra information sent with
   --  the request. For HTTP servlets, parameters are contained in the query string
   --  or posted form data.
   --
   --  You should only use this method when you are sure the parameter has only one
   --  value. If the parameter might have more than one value, use
   --  Get_Parameter_Values(String).
   --
   --  If you use this method with a multivalued parameter, the value returned is
   --  equal to the first value in the array returned by Get_Parameter_Values.
   --
   --  If the parameter data was sent in the request body, such as occurs with
   --  an HTTP POST request, then reading the body directly via getInputStream()
   --  or getReader() can interfere with the execution of this method.
   --  ------------------------------
   overriding
   function Get_Parameter (Req  : in Request;
                           Name : in String) return String is
   begin
      return Find (Req.Parameters, Name);
   end Get_Parameter;

   --  ------------------------------
   --  Iterate over the request parameters and executes the <b>Process</b> procedure.
   --  ------------------------------
   overriding
   procedure Iterate_Parameters (Req     : in Request;
                                 Process : not null access
                                   procedure (Name  : in String;
                                              Value : in String)) is

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor);

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor) is
      begin
         Process.all (Name  => Util.Strings.Maps.Key (Position),
                      Value => Util.Strings.Maps.Element (Position));
      end Process_Wrapper;

   begin
      Req.Parameters.Iterate (Process => Process_Wrapper'Access);
   end Iterate_Parameters;

   --  ------------------------------
   --  Set the parameter
   --  ------------------------------
   procedure Set_Parameter (Req   : in out Request;
                            Name  : in String;
                            Value : in String) is
   begin
      Req.Parameters.Include (Name, Value);
   end Set_Parameter;

   --  ------------------------------
   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   --  ------------------------------
   overriding
   function Get_Method (Req : in Request) return String is
   begin
      return To_String (Req.Method);
   end Get_Method;

   --  ------------------------------
   --  Sets the HTTP method.
   --  ------------------------------
   procedure Set_Method (Req    : in out Request;
                         Method : in String) is
   begin
      Req.Method := To_Unbounded_String (Method);
   end Set_Method;

   --  ------------------------------
   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   --  ------------------------------
   overriding
   function Get_Protocol (Req : in Request) return String is
   begin
      return To_String (Req.Protocol);
   end Get_Protocol;

   --  ------------------------------
   --  Sets the protocol version
   --  ------------------------------
   procedure Set_Protocol (Req      : in out Request;
                           Protocol : in String) is
   begin
      Req.Protocol := To_Unbounded_String (Protocol);
   end Set_Protocol;

   --  ------------------------------
   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   --  ------------------------------
   overriding
   function Get_Request_URI (Req : in Request) return String is
   begin
      return To_String (Req.URI);
   end Get_Request_URI;

   --  ------------------------------
   --  Set the request URI.  When <tt>Split</tt> is true, the request parameters are
   --  cleared and initialized with the query parameters passed in the URI.
   --  ------------------------------
   procedure Set_Request_URI (Req   : in out Request;
                              URI   : in String;
                              Split : in Boolean := False) is
   begin
      if not Split then
         Req.URI := To_Unbounded_String (URI);
      else
         Req.Parameters.Clear;
         declare
            Sep       : Natural := Util.Strings.Index (URI, '?');
            Sep2, Pos : Natural;
         begin
            if Sep = 0 then
               Pos := URI'Last;
               Req.URI := To_Unbounded_String (URI);
            else
               Pos := Sep + 1;
               Req.URI := To_Unbounded_String (URI (URI'First .. Sep - 1));
            end if;

            --  Do a simple parameter identification and split.
            --  Since this is for a mockup, we don't need full compliance.
            while Pos < URI'Last loop
               Sep := Util.Strings.Index (URI, '=', Pos);
               if Sep = 0 then
                  Req.Set_Parameter (URI (Pos .. URI'Last), "");
                  exit;
               end if;
               Sep2 := Util.Strings.Index (URI, '&', Sep + 1);
               if Sep2 = 0 then
                  Req.Set_Parameter (URI (Pos .. Sep - 1), URI (Sep + 1 .. URI'Last));
                  exit;
               end if;
               Req.Set_Parameter (URI (Pos .. Sep - 1), URI (Sep + 1 .. Sep2 - 1));
               Pos := Sep2 + 1;
            end loop;
         end;
      end if;
   end Set_Request_URI;

   --  ------------------------------
   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   --  ------------------------------
   overriding
   function Get_Header (Req  : in Request;
                        Name : in String) return String is
   begin
      return Find (Req.Headers, Name);
   end Get_Header;

   --  ------------------------------
   --  Sets the header
   --  ------------------------------
   procedure Set_Header (Req   : in out Request;
                         Name  : in String;
                         Value : in String) is
   begin
      Req.Headers.Include (Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Returns all the values of the specified request header as an Enumeration
   --  of String objects.
   --
   --  Some headers, such as Accept-Language can be sent by clients as several headers
   --  each with a different value rather than sending the header as a comma
   --  separated list.
   --
   --  If the request did not include any headers of the specified name, this method
   --  returns an empty Enumeration. The header name is case insensitive. You can use
   --  this method with any request header.
   --  ------------------------------
   overriding
   function Get_Headers (Req  : in Request;
                         Name : in String) return String is
   begin
      return Find (Req.Headers, Name);
   end Get_Headers;

   --  ------------------------------
   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   overriding
   procedure Iterate_Headers (Req     : in Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor);

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor) is
      begin
         Process.all (Name  => Util.Strings.Maps.Key (Position),
                      Value => Util.Strings.Maps.Element (Position));
      end Process_Wrapper;

   begin
      Req.Headers.Iterate (Process => Process_Wrapper'Access);
   end Iterate_Headers;

   --  ------------------------------
   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   --  ------------------------------
   overriding
   function Get_Remote_Addr (Req : in Request) return String is
   begin
      return To_String (Req.Peer);
   end Get_Remote_Addr;

   --  ------------------------------
   --  Sets the peer address
   --  ------------------------------
   procedure Set_Remote_Addr (Req  : in out Request;
                              Addr : in String) is
   begin
      Req.Peer := To_Unbounded_String (Addr);
   end Set_Remote_Addr;

   --  ------------------------------
   --  Get the number of parts included in the request.
   --  ------------------------------
   overriding
   function Get_Part_Count (Req : in Request) return Natural is
      pragma Unreferenced (Req);
   begin
      return 0;
   end Get_Part_Count;

   --  ------------------------------
   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   overriding
   procedure Process_Part (Req      : in out Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class)) is
   begin
      null;
   end Process_Part;

   --  ------------------------------
   --  Process the part identified by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   overriding
   procedure Process_Part (Req      : in out Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class)) is
   begin
      null;
   end Process_Part;

   --  ------------------------------
   --  Set the request cookie by using the cookie returned in the response.
   --  ------------------------------
   procedure Set_Cookie (Req  : in out Request;
                         From : in Servlet.Responses.Mockup.Response'Class) is
      C : constant String := From.Get_Header ("Set-Cookie");
   begin
      Req.Set_Header ("Cookie", C);
   end Set_Cookie;

   --  ------------------------------
   --  Set the session associated with the request.
   --  ------------------------------
   procedure Set_Session (Req     : in out Request;
                          Session : in Servlet.Sessions.Session) is
   begin
      Req.Info.Session := Session;
   end Set_Session;

   --  ------------------------------
   --  Set a part to the request.
   --  ------------------------------
   procedure Set_Part (Req          : in out Part_Request;
                       Position     : in Positive;
                       Name         : in String;
                       Path         : in String;
                       Content_Type : in String) is
   begin
      Servlet.Parts.Mockup.Create (Req.Parts (Position), Name, Path, Content_Type);
   end Set_Part;

   --  ------------------------------
   --  Get the number of parts included in the request.
   --  ------------------------------
   overriding
   function Get_Part_Count (Req : in Part_Request) return Natural is
   begin
      return Req.Count;
   end Get_Part_Count;

   --  ------------------------------
   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   overriding
   procedure Process_Part (Req      : in out Part_Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class)) is
   begin
      Process (Req.Parts (Position));
   end Process_Part;

   --  ------------------------------
   --  Process the part identified by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   overriding
   procedure Process_Part (Req      : in out Part_Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class)) is
   begin
      for I in 1 .. Req.Count loop
         if Id = Req.Parts (I).Get_Name then
            Process (Req.Parts (I));
         end if;
      end loop;
   end Process_Part;

end Servlet.Requests.Mockup;
