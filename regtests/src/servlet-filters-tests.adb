-----------------------------------------------------------------------
--  Filters Tests - Unit tests for Servlet.Filters
--  Copyright (C) 2015, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Servlet.Filters.Tests is

   --  ------------------------------
   --  Increment the counter each time Do_Filter is called.
   --  ------------------------------
   overriding
   procedure Do_Filter (F        : in Test_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain) is
      use type Core.Servlet_Registry_Access;
      Registry : constant Core.Servlet_Registry_Access := Core.Get_Servlet_Context (Chain);
   begin
      if Registry /= null then
         F.Count.all := F.Count.all + 1;
      end if;
      Servlet.Core.Do_Filter (Chain => Chain, Request => Request, Response => Response);
   end Do_Filter;

   --  ------------------------------
   --  Initialize the test filter.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Test_Filter;
                         Config  : in Servlet.Core.Filter_Config) is
      Name : constant String := Core.Get_Filter_Name (Config);
   begin
      if Name = "F1" or else Name = "F2" then
         Server.Count := Server.Counter'Unchecked_Access;
      end if;
   end Initialize;

end Servlet.Filters.Tests;
