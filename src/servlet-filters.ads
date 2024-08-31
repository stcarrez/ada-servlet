-----------------------------------------------------------------------
--  servlet-filters -- Servlet Filters
--  Copyright (C) 2010, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Core;

--  The <b>Servlet.Filters</b> package defines the servlet filter
--  interface described in Java Servlet Specification, JSR 315, 6. Filtering.
--
package Servlet.Filters is

   --  ------------------------------
   --  Filter interface
   --  ------------------------------
   --  The <b>Filter</b> interface defines one mandatory procedure through
   --  which the request/response pair are passed.
   --
   --  The <b>Filter</b> instance must be registered in the <b>Servlet_Registry</b>
   --  by using the <b>Add_Filter</b> procedure.  The same filter instance is used
   --  to process multiple requests possibly at the same time.
   type Filter is limited interface;
   type Filter_Access is access all Filter'Class;
   type Filter_List is array (Natural range <>) of Filter_Access;
   type Filter_List_Access is access all Filter_List;

   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The Filter_Chain passed in to this
   --  method allows the Filter to pass on the request and response to the next
   --  entity in the chain.
   --
   --  A typical implementation of this method would follow the following pattern:
   --  1. Examine the request
   --  2. Optionally wrap the request object with a custom implementation to
   --     filter content or headers for input filtering
   --  3. Optionally wrap the response object with a custom implementation to
   --     filter content or headers for output filtering
   --  4. Either invoke the next entity in the chain using the FilterChain
   --     object (chain.Do_Filter()),
   --     or, not pass on the request/response pair to the next entity in the
   --     filter chain to block the request processing
   --  5. Directly set headers on the response after invocation of the next
   --     entity in the filter chain.
   procedure Do_Filter (F        : in Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain) is abstract;

   --  Called by the servlet container to indicate to a filter that the filter
   --  instance is being placed into service.
   procedure Initialize (Server  : in out Filter;
                         Config  : in Servlet.Core.Filter_Config) is null;

end Servlet.Filters;
