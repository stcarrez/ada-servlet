-----------------------------------------------------------------------
--  servlet-streams -- Print streams for servlets
--  Copyright (C) 2010, 2011, 2012, 2013, 2017, 2018, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;

with Util.Streams;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with EL.Objects;
package Servlet.Streams is

   subtype Input_Stream is Util.Streams.Buffered.Input_Buffer_Stream;
   type Input_Stream_Access is access all Input_Stream'Class;

   --  -----------------------
   --  Print stream
   --  -----------------------
   --  The <b>Print_Stream</b> is an output stream which provides helper methods
   --  for writing text streams.
   type Print_Stream is new Ada.Finalization.Limited_Controlled
     and Util.Streams.Output_Stream with private;

   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Util.Streams.Texts.Print_Stream_Access);

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Print_Stream'Class);

   --  Write an integer on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer);

   --  Write a string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write a string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   --  Write the object converted into a string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in EL.Objects.Object);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Print_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   overriding
   procedure Flush (Stream : in out Print_Stream);

   --  Write into the text stream.
   procedure Write (Stream : in out Print_Stream;
                    Print  : access procedure
                      (Into : in out Util.Streams.Texts.Print_Stream'Class));

   --  Close the text stream.
   overriding
   procedure Close (Stream : in out Print_Stream);

private

   type Print_Stream is new Ada.Finalization.Limited_Controlled
     and Util.Streams.Output_Stream with record
      Target : Util.Streams.Texts.Print_Stream_Access;
   end record;

end Servlet.Streams;
