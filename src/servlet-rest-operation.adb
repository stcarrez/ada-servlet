-----------------------------------------------------------------------
--  servlet-rest-operation -- REST API Operation Definition
--  Copyright (C) 2017, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Servlet.Rest.Operation is

   URI_Mapping : aliased String := URI;
   Desc        : aliased Static_Descriptor
      := (Next       => null,
          Method     => Method,
          Handler    => Handler,
          Pattern    => URI_Mapping'Access,
          Mimes      => Mimes,
          Permission => Permission,
          Streams     => Streams);

   function Definition return Descriptor_Access is
   begin
      return Desc'Access;
   end Definition;

end Servlet.Rest.Operation;
