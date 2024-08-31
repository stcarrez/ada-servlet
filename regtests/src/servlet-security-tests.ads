-----------------------------------------------------------------------
--  servlet-security-tests - Unit tests for Servlet.Security
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Servlet.Security.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the security filter granting permission for a given URI.
   procedure Test_Security_Filter (T : in out Test);

   --  Test the security filter grants access to anonymous allowed pages.
   procedure Test_Anonymous_Access (T : in out Test);

private

   --  Check that the given URI reports the HTTP status.
   procedure Check_Security (T      : in out Test;
                             URI    : in String;
                             Result : in Natural);

end Servlet.Security.Tests;
