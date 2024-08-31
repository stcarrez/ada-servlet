-----------------------------------------------------------------------
--  servlet-streams-json -- JSON Print streams for servlets
--  Copyright (C) 2016, 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Servlet.Streams.Raw is

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Servlet.Streams.Print_Stream'Class) is
   begin
      Stream.Stream := To.Target;
   end Initialize;

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
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Print_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write_Wide (Value);
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Integer) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write ((if Value then "true" else "false"));
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Util.Beans.Objects.To_String (Value));
   end Write_Attribute;

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Print_Stream;
                                   Name   : in String) is
      pragma Unreferenced (Name);
   begin
      null;
   end Write_Null_Attribute;

   --  Write the entity value.
   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in String) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Entity;

   overriding
   procedure Write_Wide_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write_Wide (Value);
   end Write_Wide_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write ((if Value then "true" else "false"));
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Integer) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Long_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Float) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Long_Long_Float'Image (Value));
   end Write_Long_Entity;

   overriding
   procedure Write_Enum_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in String) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Value);
   end Write_Enum_Entity;

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Name);
   begin
      Stream.Stream.Write (Util.Beans.Objects.To_String (Value));
   end Write_Entity;

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Print_Stream;
                                Name   : in String) is
   begin
      null;
   end Write_Null_Entity;

end Servlet.Streams.Raw;
