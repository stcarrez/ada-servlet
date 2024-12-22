-----------------------------------------------------------------------
--  servlet-samples_servers - Tests the samples
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Servlet.Server;

generic
   Path : String;
   Port : Servlet.Server.Port_Number;
package Servlet.Samples_Servers is

   function Get_URI return String;

   type Test is new Util.Tests.Test with null record;

   overriding
   procedure Set_Up (T : in out Test);

   overriding
   procedure Tear_Down (T : in out Test);

end Servlet.Samples_Servers;
