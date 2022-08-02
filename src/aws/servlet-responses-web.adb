-----------------------------------------------------------------------
--  servlet-responses.web -- Servlet Responses with AWS server
--  Copyright (C) 2009, 2010, 2011, 2017, 2018, 2022 Stephane Carrez
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

with AWS.Headers;
with AWS.Messages;
with AWS.Response.Set;
with AWS.Containers.Tables;
package body Servlet.Responses.Web is

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
   begin
      AWS.Response.Set.Append_Body (D    => Stream.Data,
                                    Item => Buffer);
   end Write;

   --  ------------------------------
   --  Flush the buffer (if any) to the sink.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Response) is
   begin
      null;
   end Flush;

   function To_Status_Code (Status : in Natural) return AWS.Messages.Status_Code;

   function To_Status_Code (Status : in Natural) return AWS.Messages.Status_Code is
      use AWS.Messages;
   begin
      case Status is
         when 100 =>
            return S100;
         when 101 =>
            return S101;
         when 102 =>
            return S102;
         when 200 =>
            return S200;
         when 201 =>
            return S201;
         when 202 =>
            return S202;
         when 203 =>
            return S203;
         when 204 =>
            return S204;
         when 205 =>
            return S205;
         when 206 =>
            return S206;
         when 207 =>
            return S207;
         when 301 =>
            return S301;
         when 302 =>
            return S302;
         when 400 =>
            return S400;
         when 401 =>
            return S401;
         when 402 =>
            return S402;
         when 403 =>
            return S403;
         when 404 =>
            return S404;
         when 405 =>
            return S405;
         when others =>
            return S500;
      end case;
   end To_Status_Code;

   --  ------------------------------
   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   overriding
   procedure Iterate_Headers (Resp    : in Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
      Headers : constant AWS.Headers.List := AWS.Response.Header (Resp.Data);
   begin
      AWS.Containers.Tables.Iterate_Names (AWS.Containers.Tables.Table_Type (Headers),
                                           ";", Process);
   end Iterate_Headers;

   --  ------------------------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   overriding
   function Contains_Header (Resp : in Response;
                             Name : in String) return Boolean is
   begin
      raise Program_Error with "Contains_Header is not implemented";
      return False;
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
      if AWS.Response.Header (Resp.Data, Name)'Length = 0 then
         AWS.Response.Set.Add_Header (D     => Resp.Data,
                                      Name  => Name,
                                      Value => Value);
      end if;
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
      AWS.Response.Set.Add_Header (D     => Resp.Data,
                                   Name  => Name,
                                   Value => Value);
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
   begin
      if not Resp.Redirect then
         AWS.Response.Set.Content_Type (D     => Resp.Data,
                                        Value => Resp.Get_Content_Type);
         Resp.Content.Flush;
         if AWS.Response.Is_Empty (Resp.Data) then
            AWS.Response.Set.Message_Body (Resp.Data, "");
         end if;
      else
         AWS.Response.Set.Mode (D     => Resp.Data,
                                Value => AWS.Response.Header);
      end if;
      AWS.Response.Set.Status_Code (D     => Resp.Data,
                                    Value => To_Status_Code (Resp.Get_Status));
   end Build;

   --  ------------------------------
   --  Get the response data
   --  ------------------------------
   function Get_Data (Resp : in Response) return AWS.Response.Data is
   begin
      return Resp.Data;
   end Get_Data;

end Servlet.Responses.Web;
