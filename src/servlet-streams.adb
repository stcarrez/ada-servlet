-----------------------------------------------------------------------
--  Servlet.Streams -- Print streams for servlets
--  Copyright (C) 2010, 2011, 2012, 2013, 2018, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Servlet.Streams is

   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Util.Streams.Texts.Print_Stream_Access) is
   begin
      Stream.Target := To;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream
   --  ------------------------------
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Print_Stream'Class) is
   begin
      Stream.Target := To.Target;
   end Initialize;

   --  ------------------------------
   --  Write an integer on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write the object converted into a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in EL.Objects.Object) is
   begin
      Stream.Target.Write (EL.Objects.To_String (Item));
   end Write;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Print_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Target.Write (Buffer);
   end Write;

   --  ------------------------------
   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Print_Stream) is
   begin
      Stream.Target.Flush;
   end Flush;

   --  ------------------------------
   --  Close the text stream.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Print_Stream) is
   begin
      Stream.Target.Close;
   end Close;

   --  ------------------------------
   --  Write into the text stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Print  : access procedure
                      (Into : in out Util.Streams.Texts.Print_Stream'Class)) is
   begin
      Print (Into => Stream.Target.all);
   end Write;

end Servlet.Streams;
