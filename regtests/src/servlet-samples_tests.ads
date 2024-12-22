-----------------------------------------------------------------------
--  servlet-samples_tests - Tests the samples
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Servlet.Samples_Servers;
generic
   Prefix : String;
package Servlet.Samples_Tests is

   Server_Volume_Path : constant String := "bin/" & Prefix & "_volume_server";
   Server_Upload_Path : constant String := "bin/" & Prefix & "_upload_server";
   Server_Api_Path    : constant String := "bin/" & Prefix & "_api_server";

   package Server_Volume_Tests is new Samples_Servers (Server_Volume_Path, 8090);
   package Server_Upload_Tests is new Samples_Servers (Server_Upload_Path, 8091);
   package Server_Api_Tests is new Samples_Servers (Server_Api_Path, 8092);

   type Volume_Test is new Server_Volume_Tests.Test with null record;

   procedure Test_Volume (T : in out Volume_Test);

   type Upload_Test is new Server_Upload_Tests.Test with null record;

   procedure Test_Upload (T : in out Upload_Test);

   type Api_Test is new Server_Api_Tests.Test with null record;

   procedure Test_Monitor (T : in out Api_Test);

   procedure Add_Tests (Suite  : in Util.Tests.Access_Test_Suite);

end Servlet.Samples_Tests;
