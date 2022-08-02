-----------------------------------------------------------------------
--  servlet-filters-cache_control -- HTTP response Cache-Control settings
--  Copyright (C) 2015, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Servlet.Requests;
with Servlet.Responses;
with Servlet.Core;

--  The <b>Servlet.Filters.Cache_Control</b> package implements a servlet filter to add
--  cache control headers in a response.
--
package Servlet.Filters.Cache_Control is

   --  Filter configuration parameter, when not empty, add a Vary header in the response.
   VARY_HEADER_PARAM    : constant String := "header.vary";

   --  Filter configuration parameter, defines the expiration date in seconds relative
   --  to the current date.  When 0, disable browser caching.
   CACHE_CONTROL_PARAM  : constant String := "header.cache-control";

   type Cache_Control_Filter is new Servlet.Filters.Filter with record
      Vary                 : Ada.Strings.Unbounded.Unbounded_String;
      Cache_Control_Header : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The <b>Cache_Control</b> filter adds
   --  a <tt>Cache-Control</tt>, <tt>Expires</tt>, <tt>Pragma</tt> and optionally a
   --  <tt>Vary</tt> header in the HTTP response.
   overriding
   procedure Do_Filter (F        : in Cache_Control_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain);

   --  Called by the servlet container to indicate to a filter that the filter
   --  instance is being placed into service.
   overriding
   procedure Initialize (Server  : in out Cache_Control_Filter;
                         Config  : in Servlet.Core.Filter_Config);

end Servlet.Filters.Cache_Control;
