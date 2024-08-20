-----------------------------------------------------------------------
--  servlet-responses-ews -- Servlet Responses with EWS server
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Streams;

with EWS.HTTP; use EWS;
with EWS.Dynamic;
with Util.Strings.Maps;
with Util.Streams;
with Util.Streams.Texts;
package Servlet.Responses.EWS is

   type Dynamic_Response (To : HTTP.Request_P) is new Dynamic.Dynamic_Response with private;

   --  You do not need to override Content_Length and Write_Content
   --  provided you have overridden Content.

   overriding
   function Response_Kind (This : Dynamic_Response) return String;
   --  default "200 OK"

   overriding
   function Cacheable (This : Dynamic_Response) return Boolean;
   --  default True

   overriding
   function Content_Type (This : Dynamic_Response) return String;
   --  default "text/plain"

   overriding
   function Headers (This : Dynamic_Response) return String;

   type Response (To : HTTP.Request_P) is
     new Servlet.Responses.Response and Util.Streams.Output_Stream with private;

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Response;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer (if any) to the sink.
   overriding
   procedure Flush (Stream : in out Response);

   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   overriding
   procedure Iterate_Headers (Resp    : in Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   overriding
   function Contains_Header (Resp : in Response;
                             Name : in String) return Boolean;

   --  Sets a response header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   overriding
   procedure Set_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String);

   --  Adds a response header with the given name and value.
   --  This method allows response headers to have multiple values.
   overriding
   procedure Add_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String);

   --  Sends a temporary redirect response to the client using the specified redirect
   --  location URL. This method can accept relative URLs; the servlet container must
   --  convert the relative URL to an absolute URL before sending the response to the
   --  client. If the location is relative without a leading '/' the container
   --  interprets it as relative to the current request URI. If the location is relative
   --  with a leading '/' the container interprets it as relative to the servlet
   --  container root.
   --
   --  If the response has already been committed, this method throws an
   --  IllegalStateException. After using this method, the response should be
   --  considered to be committed and should not be written to.
   overriding
   procedure Send_Redirect (Resp     : in out Response;
                            Location : in String);

   --  Prepare the response data by collecting the status, content type and message body.
   procedure Build (Resp : in out Response);

   --  Get the response data
   function Get_Data (Resp : in Response) return Dynamic_Response'Class;

private

   use Ada.Strings.Unbounded;

   overriding
   procedure Initialize (Resp : in out Response);

   type Dynamic_Response (To : HTTP.Request_P) is new Dynamic.Dynamic_Response (To) with record
      Content_Type : Unbounded_String;
      Headers      : Unbounded_String;
      Status       : Integer := 0;
   end record;

   type Response (To : HTTP.Request_P) is
     new Servlet.Responses.Response and Util.Streams.Output_Stream with record
      Content  : aliased Util.Streams.Texts.Print_Stream;
      Redirect : Boolean := False;
      Reply    : Dynamic_Response (To);
      Headers  : Util.Strings.Maps.Map;
   end record;

end Servlet.Responses.EWS;
