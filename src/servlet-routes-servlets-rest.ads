-----------------------------------------------------------------------
--  servlet-routes-servlets-rest -- Route for the REST API
--  Copyright (C) 2016 Stephane Carrez
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
with Servlet.Rest;

package Servlet.Routes.Servlets.Rest is

   type Descriptor_Array is array (Servlet.Rest.Method_Type) of Servlet.Rest.Descriptor_Access;

   --  The route has one descriptor for each REST method.
   type API_Route_Type is new Servlet.Routes.Servlets.Servlet_Route_Type with record
      Descriptors : Descriptor_Array;
   end record;
   type API_Route_Type_Access is access all API_Route_Type'Class;

end Servlet.Routes.Servlets.Rest;
