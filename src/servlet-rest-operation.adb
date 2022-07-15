-----------------------------------------------------------------------
--  servlet-rest-operation -- REST API Operation Definition
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
package body Servlet.Rest.Operation is

   URI_Mapping : aliased String := URI;
   Desc        : aliased Static_Descriptor
      := (Next       => null,
          Method     => Method,
          Handler    => Handler,
          Pattern    => URI_Mapping'Access,
          Mimes      => Mimes,
          Permission => Permission);

   function Definition return Descriptor_Access is
   begin
      return Desc'Access;
   end Definition;

end Servlet.Rest.Operation;
