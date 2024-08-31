-----------------------------------------------------------------------
--  servlet-responses.tools -- Servlet Responses Tools
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Servlet.Responses.Tools is

   --  Builds a printable representation of the response for debugging purposes.
   --  When <b>Html</b> is true, the returned content contains an HTML presentation.
   function To_String (Reply            : in Response'Class;
                       Html             : in Boolean := False;
                       Print_Headers    : in Boolean := True) return String;

end Servlet.Responses.Tools;
