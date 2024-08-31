-----------------------------------------------------------------------
--  servlet-streams-xml -- XML Print streams for servlets
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.IO.XML;
package Servlet.Streams.XML is

   subtype Print_Stream is Util.Serialize.IO.XML.Output_Stream;

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Servlet.Streams.Print_Stream'Class);

end Servlet.Streams.XML;
