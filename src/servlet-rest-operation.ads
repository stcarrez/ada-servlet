-----------------------------------------------------------------------
--  servlet-rest-operation -- REST API Operation Definition
--  Copyright (C) 2017, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Streams.Dynamic;
generic
   Handler    : Operation_Access;
   Method     : Method_Type := GET;
   URI        : String;
   Permission : Security.Permissions.Permission_Index := Security.Permissions.NONE;
   Mimes      : Mime_List_Access := null;
   Streams    : Stream_Modes := (Servlet.Streams.Dynamic.DYNAMIC => False, others => True);
package Servlet.Rest.Operation is

   function Definition return Descriptor_Access;

end Servlet.Rest.Operation;
