-----------------------------------------------------------------------
--  servlet-responses.tools -- Servlet Responses Tools
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Util.Strings.Transforms;
package body Servlet.Responses.Tools is

   --  ------------------------------
   --  Builds a printable representation of the response for debugging purposes.
   --  When <b>Html</b> is true, the returned content contains an HTML presentation.
   --  ------------------------------
   function To_String (Reply            : in Response'Class;
                       Html             : in Boolean := False;
                       Print_Headers    : in Boolean := True) return String is

      use Ada.Strings.Unbounded;

      procedure Put (Title : in String; Value : in String);
      procedure Append_Html (Content : in String);
      pragma Inline (Append_Html);

      Info : Unbounded_String;

      procedure Append_Html (Content : in String) is
      begin
         if Html then
            Append (Info, Content);
         end if;
      end Append_Html;

      procedure Put (Title : in String;
                     Value : in String) is
      begin
         if Html then
            Append (Info, "<tr><td>");
            Util.Strings.Transforms.Escape_Xml (Content => Title,
                                                Into    => Info);
            Append (Info, "</td><td>");
            Util.Strings.Transforms.Escape_Xml (Content => Value,
                                                Into    => Info);
            Append (Info, "</td></tr>");
         else
            Append (Info, Title);
            Append (Info, ": ");
            Append (Info, Value);
            Append (Info, ASCII.LF);
         end if;
      end Put;

   begin
      Append_Html ("<div class='servlet-dbg-req'><div class='servlet-dbg-uri'>"
                   & "<table class='servlet-dbg-uri'><tr><th colspan='2'>Response</th></tr>");
      Append (Info, ASCII.LF);
      Put ("      Status", Natural'Image (Reply.Get_Status));
      Put ("Content-Type", Reply.Get_Content_Type);
      Append_Html ("</table></div>");

      if Print_Headers then
         Append_Html ("<div class='servlet-dbg-attr'><table class='servlet-dbg-list'>"
                      & "<tr><th colspan='2'>Headers</th></tr>");
         Reply.Iterate_Headers (Process => Put'Access);
         Append_Html ("</table></div>");
      end if;

      Append_Html ("</div>");
      return To_String (Info);
   end To_String;

end Servlet.Responses.Tools;
