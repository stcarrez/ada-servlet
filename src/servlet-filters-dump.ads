-----------------------------------------------------------------------
--  servlet-filters.dump -- Filter to dump the request information
--  Copyright (C) 2010, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Core;

--  The <b>Servlet.Filters.Dump</b> package provides a debugging filter which
--  can be activated in the request flow to dump the request content into
--  some log file before processing the request.
package Servlet.Filters.Dump is

   type Dump_Filter is new Servlet.Filters.Filter with null record;

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
   overriding
   procedure Do_Filter (F        : in Dump_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain);

end Servlet.Filters.Dump;
