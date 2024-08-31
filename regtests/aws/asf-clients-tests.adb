-----------------------------------------------------------------------
--  asf-clients-tests - Unit tests for HTTP clients
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Log.Loggers;

with ASF.Clients.Web;
package body ASF.Clients.Tests is

   use Util.Tests;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Clients.Tests");

   package Caller is new Util.Test_Caller (Test, "Clients");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Clients.Do_Get",
                       Test_Http_Get'Access);
      ASF.Clients.Web.Register;
   end Add_Tests;

   --  ------------------------------
   --  Test creation of cookie
   --  ------------------------------
   procedure Test_Http_Get (T : in out Test) is
      pragma Unreferenced (T);

      C     : ASF.Clients.Client;
      Reply : ASF.Clients.Response;
   begin
      C.Do_Get (URL   => "http://www.google.com/", Reply => Reply);
      Log.Info ("Result: {0}", Reply.Get_Body);
   end Test_Http_Get;

end ASF.Clients.Tests;
