-----------------------------------------------------------------------
--  asf-clients-tests - Unit tests for HTTP clients
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package ASF.Clients.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test creation of cookie
   procedure Test_Http_Get (T : in out Test);

end ASF.Clients.Tests;
