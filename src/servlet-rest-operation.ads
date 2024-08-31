-----------------------------------------------------------------------
--  servlet-rest-operation -- REST API Operation Definition
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
generic
   Handler    : Operation_Access;
   Method     : Method_Type := GET;
   URI        : String;
   Permission : Security.Permissions.Permission_Index := Security.Permissions.NONE;
   Mimes      : Mime_List_Access := null;
package Servlet.Rest.Operation is

   function Definition return Descriptor_Access;

end Servlet.Rest.Operation;
