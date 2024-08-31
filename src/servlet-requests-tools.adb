-----------------------------------------------------------------------
--  servlet-requests.tools -- Servlet Requests Tools
--  Copyright (C) 2010, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Strings.Transforms;
package body Servlet.Requests.Tools is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Builds a printable representation of the request for debugging purposes.
   --  When <b>Html</b> is true, the returned content contains an HTML presentation.
   --  ------------------------------
   function To_String (Req              : in Request'Class;
                       Html             : in Boolean := False;
                       Print_Headers    : in Boolean := True;
                       Print_Attributes : in Boolean := False) return String is

      procedure Put (Title : in String; Value : in String);
      procedure Put (Name  : in String; Value : in EL.Objects.Object);
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

      procedure Put (Name  : in String;
                     Value : in EL.Objects.Object) is
      begin
         Put (Title => Name, Value => EL.Objects.To_String (Value));
      end Put;

   begin
      Append_Html ("<div class='servlet-dbg-req'><div class='servlet-dbg-uri'>"
                   & "<table class='servlet-dbg-uri'><tr><th colspan='2'>Request</th></tr>");
      Append (Info, ASCII.LF);
      Put ("     URI", Req.Get_Request_URI);
      Put ("    Peer", Req.Get_Remote_Host);
      Put ("Protocol", Req.Get_Protocol);
      Put ("  Method", Req.Get_Method);
      Put ("   Query", Req.Get_Query_String);
      Append_Html ("</table></div>");

      if Print_Headers then
         Append_Html ("<div class='servlet-dbg-attr'><table class='servlet-dbg-list'>"
                      & "<tr><th colspan='2'>Headers</th></tr>");
         Req.Iterate_Headers (Process => Put'Access);
         Append_Html ("</table></div>");
      end if;

      if Print_Attributes then
         Append_Html ("<div class='servlet-dbg-attr'><table class='servlet-dbg-list'>"
                      & "<tr><th colspan='2'>Attributes</th></tr>");
         Req.Iterate_Attributes (Process => Put'Access);
         Append_Html ("</table></div>");
      end if;

      Append_Html ("</div>");
      return To_String (Info);
   end To_String;

   --  ------------------------------
   --  Set the internal context associated with a request:
   --  <ul>
   --     <li>The servlet that processes the request,
   --     <li>The response associated with the request
   --  </ul/
   --  ------------------------------
   procedure Set_Context (Req      : in out Request'Class;
                          Response : in Servlet.Responses.Response_Access;
                          Context  : access Servlet.Routes.Route_Context_Type) is
   begin
      Req.Context := Context;
      Req.Info.Response := Response;
   end Set_Context;

end Servlet.Requests.Tools;
