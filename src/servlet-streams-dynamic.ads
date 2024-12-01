-----------------------------------------------------------------------
--  servlet-streams-dynamic -- Print streams for REST API
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar;
with Ada.Finalization;
with Util.Beans.Objects;
with Util.Serialize.IO;
with Util.Http.Mimes;
package Servlet.Streams.Dynamic is

   --  The supported stream types for the operation.
   type Stream_Type is (JSON, XML, FORM, RAW, DYNAMIC);

   type Print_Stream is limited new Ada.Finalization.Limited_Controlled
     and Util.Serialize.IO.Output_Stream with private;

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Util.Streams.Texts.Print_Stream_Access);

   procedure Set_JSON (Stream : in out Print_Stream);
   procedure Set_XML (Stream : in out Print_Stream);
   procedure Set_Raw (Stream : in out Print_Stream);

   procedure Set_Stream_Type (Stream : in out Print_Stream;
                              Kind   : in Stream_Type);

   overriding
   procedure Finalize (Stream : in out Print_Stream);

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

   procedure Set_Content_Type (Stream : in out Print_Stream;
                               Mime   : in Util.Http.Mimes.Mime_Access);

private

   type IO_Output_Stream_Access is access all Util.Serialize.IO.Output_Stream'Class;

   type Print_Stream is limited new Ada.Finalization.Limited_Controlled
     and Util.Serialize.IO.Output_Stream with record
      Raw_Stream : Util.Streams.Texts.Print_Stream_Access;
      Stream     : IO_Output_Stream_Access;
      Allocated  : Boolean := False;
   end record;

end Servlet.Streams.Dynamic;