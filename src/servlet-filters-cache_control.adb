-----------------------------------------------------------------------
--  servlet-filters-cache_control -- HTTP response Cache-Control settings
--  Copyright (C) 2015, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Servlet.Filters.Cache_Control is

   --  ------------------------------
   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The <b>Cache_Control</b> filter adds
   --  a <tt>Cache-Control</tt>, <tt>Expires</tt>, <tt>Pragma</tt> and optionally a
   --  <tt>Vary</tt> header in the HTTP response.
   --  ------------------------------
   overriding
   procedure Do_Filter (F        : in Cache_Control_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain) is
      use Ada.Strings.Unbounded;
   begin
      if Length (F.Cache_Control_Header) > 0 then
         Response.Add_Header ("Cache-Control", To_String (F.Cache_Control_Header));
      end if;
      if Length (F.Vary) > 0 then
         Response.Add_Header ("Vary", To_String (F.Vary));
      end if;
      Servlet.Core.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);
   end Do_Filter;

   --  ------------------------------
   --  Called by the servlet container to indicate to a filter that the filter
   --  instance is being placed into service.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Cache_Control_Filter;
                         Config  : in Servlet.Core.Filter_Config) is
   begin
      Server.Vary                 := Core.Get_Init_Parameter (Config, VARY_HEADER_PARAM);
      Server.Cache_Control_Header := Core.Get_Init_Parameter (Config, CACHE_CONTROL_PARAM);
   end Initialize;

end Servlet.Filters.Cache_Control;
