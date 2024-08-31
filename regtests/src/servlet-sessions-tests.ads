-----------------------------------------------------------------------
--  Sessions Tests - Unit tests for Servlet.Sessions
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Servlet.Sessions.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Writer    : Integer;
   end record;

   --  Test creation of session
   procedure Test_Create_Session (T : in out Test);
   procedure Test_Empty_Session (T : in out Test);
   procedure Test_Session_Attributes (T : in out Test);

end Servlet.Sessions.Tests;
