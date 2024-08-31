-----------------------------------------------------------------------
--  servlet-routes-servlets-rest -- Route for the REST API
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
