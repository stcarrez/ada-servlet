-----------------------------------------------------------------------
--  servlet-responses -- Servlet Requests
--  Copyright (C) 2010, 2011, 2015, 2018, 2021, 2022 Stephane Carrez
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

--  The <b>Servlet.Responses</b> package is an Ada implementation of
--  the Java servlet response (JSR 315 5. The Response).
package body Servlet.Responses.Mockup is

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
         Resp.Headers.Include (Name, Resp.Headers.Element (Name) & ASCII.LF
                               & Value);
      else
         Resp.Headers.Insert (Name, Value);
      end if;
   end Add_Header;

   --  ------------------------------
   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the response. The header name is case insensitive. You can use
   --  this method with any response header.
   --  ------------------------------
   function Get_Header (Resp : in Response;
                        Name : in String) return String is
      Pos : constant Util.Strings.Maps.Cursor := Resp.Headers.Find (Name);
   begin
      if Util.Strings.Maps.Has_Element (Pos) then
         return Util.Strings.Maps.Element (Pos);
      else
         return "";
      end if;
   end Get_Header;

   --  ------------------------------
   --  Get the content written to the mockup output stream.
   --  ------------------------------
   procedure Read_Content (Resp : in out Response;
                           Into : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Resp.Headers.Include ("Content-Type", Resp.Get_Content_Type);
      Resp.Content.Flush (Into);
   end Read_Content;

   --  ------------------------------
   --  Clear the response content.
   --  This operation removes any content held in the output stream, clears the status,
   --  removes any header in the response.
   --  ------------------------------
   procedure Clear (Resp : in out Response) is
      Into : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Resp.Content.Flush (Into);
      Resp.Status := SC_OK;
      Resp.Headers.Clear;
      Resp.Committed := False;
   end Clear;

   --  ------------------------------
   --  Initialize the response mockup output stream.
   --  ------------------------------
   overriding
   procedure Initialize (Resp : in out Response) is
   begin
      Resp.Content.Initialize (256 * 1024);
      Resp.Stream := Resp.Content'Unchecked_Access;
   end Initialize;

end Servlet.Responses.Mockup;
