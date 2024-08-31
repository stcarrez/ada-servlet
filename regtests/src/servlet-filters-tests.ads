-----------------------------------------------------------------------
--  Filters Tests - Unit tests for Servlet.Filters
--  Copyright (C) 2015, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Servlet.Filters.Tests is

   --  A simple filter that counts the number of times it is traversed.
   type Test_Filter is new Filter with record
      Count   : access Integer;
      Counter : aliased Integer := 0;
   end record;

   --  Increment the counter each time Do_Filter is called.
   overriding
   procedure Do_Filter (F        : in Test_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain);

   --  Initialize the test filter.
   overriding
   procedure Initialize (Server  : in out Test_Filter;
                         Config  : in Servlet.Core.Filter_Config);

end Servlet.Filters.Tests;
