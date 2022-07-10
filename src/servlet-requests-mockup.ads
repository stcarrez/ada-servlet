-----------------------------------------------------------------------
--  servlet-requests.mockup -- Servlet Requests mockup
--  Copyright (C) 2010, 2011, 2012, 2013, 2017, 2020, 2022 Stephane Carrez
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
with Util.Strings.Maps;
with Servlet.Responses.Mockup;
with Servlet.Parts.Mockup;

--  The <b>Servlet.Requests.Mockup</b> provides a fake request object to simulate
--  an HTTP request.
package Servlet.Requests.Mockup is

   --  ------------------------------
   --  Request Mockup
   --  ------------------------------
   --  The request mockup provides additional procedures to set the request
   --  parameters, the URI, the peer address and other read-only values.
   type Request is new Servlet.Requests.Request with private;
   type Request_Access is access all Request'Class;

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
   overriding
   function Get_Parameter (Req  : Request;
                           Name : String) return String;

   --  Iterate over the request parameters and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Parameters (Req     : in Request;
                                 Process : not null access
                                   procedure (Name  : in String;
                                              Value : in String));

   --  Set the parameter
   procedure Set_Parameter (Req   : in out Request;
                            Name  : in String;
                            Value : in String);

   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   overriding
   function Get_Method (Req : in Request) return String;

   --  Sets the HTTP method.
   procedure Set_Method (Req : in out Request;
                         Method : in String);

   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   overriding
   function Get_Protocol (Req : in Request) return String;

   --  Sets the protocol version
   procedure Set_Protocol (Req      : in out Request;
                           Protocol : in String);

   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   overriding
   function Get_Request_URI (Req : in Request) return String;

   --  Set the request URI.  When <tt>Split</tt> is true, the request parameters are
   --  cleared and initialized with the query parameters passed in the URI.
   procedure Set_Request_URI (Req   : in out Request;
                              URI   : in String;
                              Split : in Boolean := False);

   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   overriding
   function Get_Header (Req  : in Request;
                        Name : in String) return String;

   --  Sets the header
   procedure Set_Header (Req   : in out Request;
                         Name  : in String;
                         Value : in String);

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
   overriding
   function Get_Headers (Req  : in Request;
                         Name : in String) return String;

   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Req     : in Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   overriding
   function Get_Remote_Addr (Req : in Request) return String;

   --  Sets the peer address
   procedure Set_Remote_Addr (Req  : in out Request;
                              Addr : in String);

   --  Get the number of parts included in the request.
   overriding
   function Get_Part_Count (Req : in Request) return Natural;

   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   overriding
   procedure Process_Part (Req      : in out Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class));

   --  Process the part identified by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   overriding
   procedure Process_Part (Req      : in out Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class));

   --  Set the request cookie by using the cookie returned in the response.
   procedure Set_Cookie (Req  : in out Request;
                         From : in Servlet.Responses.Mockup.Response'Class);

   --  Set the session associated with the request.
   procedure Set_Session (Req     : in out Request;
                          Session : in Servlet.Sessions.Session);

   type Part_Request (Count : Natural) is new Request with private;

   --  Set a part to the request.
   procedure Set_Part (Req          : in out Part_Request;
                       Position     : in Positive;
                       Name         : in String;
                       Path         : in String;
                       Content_Type : in String);

   --  Get the number of parts included in the request.
   overriding
   function Get_Part_Count (Req : in Part_Request) return Natural;

   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   overriding
   procedure Process_Part (Req      : in out Part_Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class));

   --  Process the part identified by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   overriding
   procedure Process_Part (Req      : in out Part_Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in Servlet.Parts.Part'Class));

private

   use Ada.Strings.Unbounded;

   type Request is new Servlet.Requests.Request with record
      Headers    : Util.Strings.Maps.Map;
      Parameters : Util.Strings.Maps.Map;
      URI        : Unbounded_String;
      Protocol   : Unbounded_String;
      Method     : Unbounded_String;
      Peer       : Unbounded_String;
   end record;

   type Part_Request (Count : Natural) is new Request with record
      Parts      : Servlet.Parts.Mockup.Part_Array (1 .. Count);
   end record;

end Servlet.Requests.Mockup;
