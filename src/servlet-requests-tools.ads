-----------------------------------------------------------------------
--  servlet-requests.tools -- Servlet Requests Tools
--  Copyright (C) 2010, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Servlet.Requests.Tools is

   --  Builds a printable representation of the request for debugging purposes.
   --  When <b>Html</b> is true, the returned content contains an HTML presentation.
   function To_String (Req              : in Request'Class;
                       Html             : in Boolean := False;
                       Print_Headers    : in Boolean := True;
                       Print_Attributes : in Boolean := False) return String;

   --  Set the internal context associated with a request:
   --  <ul>
   --     <li>The servlet that processes the request,
   --     <li>The response associated with the request
   --  </ul/
   procedure Set_Context (Req      : in out Request'Class;
                          Response : in Servlet.Responses.Response_Access;
                          Context  : access Servlet.Routes.Route_Context_Type);

end Servlet.Requests.Tools;
