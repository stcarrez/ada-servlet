-----------------------------------------------------------------------
--  servlet-streams-json -- JSON Print streams for servlets
--  Copyright (C) 2016, 2018, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.IO.JSON;
package Servlet.Streams.JSON is

   subtype Print_Stream is Util.Serialize.IO.JSON.Output_Stream;

   type Print_Stream_Access is access all Print_Stream'Class;

   --  Initialize the stream
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Servlet.Streams.Print_Stream'Class);

end Servlet.Streams.JSON;
