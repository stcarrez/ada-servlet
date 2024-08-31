-----------------------------------------------------------------------
--  servlet-streams-xml -- XML Print streams for servlets
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Servlet.Streams.XML is

   --  ------------------------------
   --  Initialize the stream
   --  ------------------------------
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Servlet.Streams.Print_Stream'Class) is
   begin
      Stream.Initialize (To.Target);
   end Initialize;

end Servlet.Streams.XML;
