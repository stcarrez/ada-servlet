-----------------------------------------------------------------------
--  servlet-server-tests - Unit tests for server requests
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Util.Tests;
with Util.Beans.Objects;
with Servlet.Tests;

package Servlet.Server.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Initialize the test.
   overriding
   procedure Set_Up (T : in out Test);

   --  Test the Service procedure.
   procedure Test_Service (T : in out Test);

   --  Test a GET request on a static file served by the File_Servlet.
   procedure Test_Get_File (T : in out Test);

   --  Test a GET 404 error on missing static file served by the File_Servlet.
   procedure Test_Get_404 (T : in out Test);

   --  Test a GET request on the measure servlet
   procedure Test_Get_Measures (T : in out Test);

end Servlet.Server.Tests;
