-----------------------------------------------------------------------
--  servlet-streams-json -- JSON Print streams for servlets
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with Servlet.Streams.JSON;
with Servlet.Streams.XML;
with Servlet.Streams.Raw;
package body Servlet.Streams.Dynamic is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Util.Serialize.IO.Output_Stream'Class,
                                     Name   => IO_Output_Stream_Access);

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Util.Streams.Texts.Print_Stream_Access) is
   begin
      Stream.Raw_Stream := To;
   end Initialize;

   procedure Set_JSON (Stream : in out Print_Stream) is
      S : constant Streams.JSON.Print_Stream_Access := new Streams.JSON.Print_Stream;
   begin
      S.Initialize (Stream.Raw_Stream);
      Free (Stream.Stream);
      Stream.Stream := S.all'Access;
   end Set_JSON;

   procedure Set_XML (Stream : in out Print_Stream) is
      S : constant Streams.XML.Print_Stream_Access := new Streams.XML.Print_Stream;
   begin
      S.Initialize (Stream.Raw_Stream);
      Free (Stream.Stream);
      Stream.Stream := S.all'Access;
   end Set_XML;

   procedure Set_Raw (Stream : in out Print_Stream) is
      S : constant Streams.Raw.Print_Stream_Access := new Streams.Raw.Print_Stream;
   begin
      S.Initialize (Stream.Raw_Stream);
      Free (Stream.Stream);
      Stream.Stream := S.all'Access;
   end Set_Raw;

   procedure Set_Stream_Type (Stream : in out Print_Stream;
                              Kind   : in Stream_Type) is
   begin
      case Kind is
         when JSON =>
            Stream.Set_JSON;

         when XML =>
            Stream.Set_XML;

         when RAW =>
            Stream.Set_Raw;

         when others =>
            null;

      end case;
   end Set_Stream_Type;

   overriding
   procedure Finalize (Stream : in out Print_Stream) is
   begin
      if Stream.Stream /= null then
         --  Stream.Stream.End_Document;
         --  Stream.Stream.Flush;
         Free (Stream.Stream);
      end if;
   end Finalize;

   --  Flush the buffer (if any) to the sink.
   overriding
   procedure Flush (Stream : in out Print_Stream) is
   begin
      Stream.Stream.Flush;
   end Flush;

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Print_Stream) is
   begin
      Stream.Stream.Close;
   end Close;

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Print_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Stream.Write (Buffer);
   end Write;

   --  Write the attribute name/value pair.
   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in String) is
   begin
      Stream.Stream.Write_Attribute (Name, Value);
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Print_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
   begin
      Stream.Stream.Write_Wide_Attribute (Name, Value);
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Integer) is
   begin
      Stream.Stream.Write_Attribute (Name, Value);
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
   begin
      Stream.Stream.Write_Attribute (Name, Value);
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
   begin
      Stream.Stream.Write_Attribute (Name, Value);
   end Write_Attribute;

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Print_Stream;
                                   Name   : in String) is
   begin
      Stream.Stream.Write_Null_Attribute (Name);
   end Write_Null_Attribute;

   --  Write the entity value.
   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in String) is
   begin
      Stream.Stream.Write_Entity (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Wide_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is
   begin
      Stream.Stream.Write_Wide_Entity (Name, Value);
   end Write_Wide_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
   begin
      Stream.Stream.Write_Entity (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Integer) is
   begin
      Stream.Stream.Write_Entity (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time) is
   begin
      Stream.Stream.Write_Entity (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer) is
   begin
      Stream.Stream.Write_Long_Entity (Name, Value);
   end Write_Long_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Float) is
   begin
      Stream.Stream.Write_Long_Entity (Name, Value);
   end Write_Long_Entity;

   overriding
   procedure Write_Enum_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in String) is
   begin
      Stream.Stream.Write_Enum_Entity (Name, Value);
   end Write_Enum_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
   begin
      Stream.Stream.Write_Entity (Name, Value);
   end Write_Entity;

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Print_Stream;
                                Name   : in String) is
   begin
      Stream.Stream.Write_Null_Entity (Name);
   end Write_Null_Entity;

   procedure Set_Content_Type (Stream : in out Print_Stream;
                               Mime   : in Util.Http.Mimes.Mime_Access) is
   begin
      null;
   end Set_Content_Type;

end Servlet.Streams.Dynamic;
