-----------------------------------------------------------------------
--  servlet-streams-json -- JSON Print streams for servlets
--  Copyright (C) 2016, 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar;
with Util.Beans.Objects;
with Util.Serialize.IO;
package Servlet.Streams.Raw is

   type Print_Stream is limited new Util.Serialize.IO.Output_Stream with private;

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Servlet.Streams.Print_Stream'Class);

   --  Flush the buffer (if any) to the sink.
   overriding
   procedure Flush (Stream : in out Print_Stream);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Print_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Print_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Write the attribute name/value pair.
   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in String);

   overriding
   procedure Write_Wide_Attribute (Stream : in out Print_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String);

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Integer);

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Boolean);

   overriding
   procedure Write_Attribute (Stream : in out Print_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object);

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Print_Stream;
                                   Name   : in String);

   --  Write the entity value.
   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in String);

   overriding
   procedure Write_Wide_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String);

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Boolean);

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Integer);

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time);

   overriding
   procedure Write_Long_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer);

   overriding
   procedure Write_Long_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Float);

   overriding
   procedure Write_Enum_Entity (Stream : in out Print_Stream;
                                Name   : in String;
                                Value  : in String);

   overriding
   procedure Write_Entity (Stream : in out Print_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object);

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Print_Stream;
                                Name   : in String);

private

   type Print_Stream is limited new Util.Serialize.IO.Output_Stream with record
      Stream    : Util.Streams.Texts.Print_Stream_Access;
   end record;

end Servlet.Streams.Raw;
