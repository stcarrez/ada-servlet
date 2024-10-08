-----------------------------------------------------------------------
--  servlet-requests-ews -- EWS Servlet Requests
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EWS.HTTP; use EWS;
with Servlet.Streams;
package Servlet.Requests.EWS is

   type Request (Data : HTTP.Request_P) is new Servlet.Requests.Request with null record;
   type Request_Access is access all Request'Class;

   overriding
   function Get_Parameter (R : Request; Name : String) return String;

   --  Iterate over the request parameters and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Parameters (Req     : in Request;
                                 Process : not null access
                                   procedure (Name  : in String;
                                              Value : in String));

   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   overriding
   function Get_Method (Req : in Request) return String;

   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   overriding
   function Get_Protocol (Req : in Request) return String;

   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   overriding
   function Get_Request_URI (Req : in Request) return String;

   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   overriding
   function Get_Header (Req  : in Request;
                        Name : in String) return String;

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

   overriding
   function Create_Input_Stream (Req : in Request) return Servlet.Streams.Input_Stream_Access;

end Servlet.Requests.EWS;
