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
with Util.Strings;
package body Servlet.Responses.EWS is

   overriding
   function Response_Kind (This : Dynamic_Response) return String is
   begin
      case (This.Status) is
         when 200 =>
            return "200 OK";

         when 201 =>
            return "201 Created";

         when 202 =>
            return "202 Accepted";

         when 204 =>
            return "204 No content";

         when 301 =>
            return "301 Moved permanently";

         when 302 =>
            return "302 Moved temporarily";

         when 303 =>
            return "303 See other";

         when 304 =>
            return "304 Not modified";

         when 305 =>
            return "305 Use proxy";

         when 307 =>
            return "307 Temporary redirect";

         when 400 =>
            return "400 Bad request";

         when 401 =>
            return "401 Unauthorized";

         when 403 =>
            return "403 Forbidden";

         when 404 =>
            return "404 Not found";

         when 405 =>
            return "405 Method not allowed";

         when others =>
            return Util.Strings.Image (This.Status) & " Unkown";

      end case;
   end Response_Kind;

   overriding
   function Cacheable (This : Dynamic_Response) return Boolean is
      pragma Unreferenced (This);
   begin
      return True;
   end Cacheable;

   overriding
   function Content_Type (This : Dynamic_Response) return String is
   begin
      return To_String (This.Content_Type);
   end Content_Type;

   overriding
   function Headers (This : Dynamic_Response) return String is
   begin
      return To_String (This.Headers);
   end Headers;

   overriding
   procedure Initialize (Resp : in out Response) is
   begin
      Resp.Content.Initialize (Size   => 256 * 1024,
                               Output => Resp'Unchecked_Access);
      Resp.Stream := Resp.Content'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Response;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
      Content : String (1 .. Buffer'Length);
      for Content'Address use Buffer'Address;
   begin
      Stream.Reply.Append (Content);
   end Write;

   --  ------------------------------
   --  Flush the buffer (if any) to the sink.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Response) is
   begin
      null;
   end Flush;

   --  ------------------------------
   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   overriding
   procedure Iterate_Headers (Resp    : in Response;
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
      Resp.Headers.Iterate (Process => Process_Wrapper'Access);
   end Iterate_Headers;

   --  ------------------------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   overriding
   function Contains_Header (Resp : in Response;
                             Name : in String) return Boolean is
      Pos : constant Util.Strings.Maps.Cursor := Resp.Headers.Find (Name);
   begin
      return Util.Strings.Maps.Has_Element (Pos);
   end Contains_Header;

   --  ------------------------------
   --  Sets a response header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   --  ------------------------------
   overriding
   procedure Set_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String) is
   begin
      Resp.Headers.Include (Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Adds a response header with the given name and value.
   --  This method allows response headers to have multiple values.
   --  ------------------------------
   overriding
   procedure Add_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String) is
   begin
      if Resp.Headers.Contains (Name) then
         Resp.Headers.Include (Name, Resp.Headers.Element (Name) & ASCII.CR & ASCII.LF
                               & Value);
      else
         Resp.Headers.Insert (Name, Value);
      end if;
   end Add_Header;

   --  ------------------------------
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
   --  ------------------------------
   overriding
   procedure Send_Redirect (Resp     : in out Response;
                            Location : in String) is
   begin
      Response'Class (Resp).Set_Status (SC_FOUND);
      Response'Class (Resp).Set_Header (Name  => "Location",
                                        Value => Location);
      Resp.Redirect := True;
   end Send_Redirect;

   --  ------------------------------
   --  Prepare the response data by collecting the status, content type and message body.
   --  ------------------------------
   procedure Build (Resp : in out Response) is
      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor);

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor) is
         Name : constant String := Util.Strings.Maps.Key (Position);
         Value : constant String := Util.Strings.Maps.Element (Position);
      begin
         Append (Resp.Reply.Headers, Name);
         Append (Resp.Reply.Headers, ": ");
         Append (Resp.Reply.Headers, Value);
         Append (Resp.Reply.Headers, ASCII.CR & ASCII.LF);
      end Process_Wrapper;
   begin
      Resp.Reply.Status := Resp.Status;
      if not Resp.Redirect then
         Resp.Content.Flush;
         Resp.Reply.Content_Type := Resp.Content_Type;
         Append (Resp.Reply.Headers, Dynamic.Dynamic_Response (Resp.Reply).Headers);
      end if;
      if not Resp.Headers.Is_Empty then
         Resp.Headers.Iterate (Process => Process_Wrapper'Access);
      end if;
   end Build;

   --  ------------------------------
   --  Get the response data
   --  ------------------------------
   function Get_Data (Resp : in Response) return Dynamic_Response'Class is
   begin
      return Resp.Reply;
   end Get_Data;

end Servlet.Responses.EWS;
