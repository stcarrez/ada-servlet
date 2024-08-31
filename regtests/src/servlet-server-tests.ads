-----------------------------------------------------------------------
--  servlet-server-tests - Unit tests for server requests
--  Copyright (C) 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

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

   --  Test a POST on a file served by the File_Servlet.
   procedure Test_Post_File_Error (T : in out Test);

   --  Test a POST with a part file to a test servlet.
   procedure Test_Post_Content (T : in out Test);

   --  Test a GET request on servlet that raises an exception.
   procedure Test_Get_With_Exception (T : in out Test);

   --  Test a Register_Application and Remove_Application.
   procedure Test_Register_Remove_Application (T : in out Test);

   --  Test a Register_Application with an empty URI (catch all requests).
   procedure Test_Register_Application (T : in out Test);

end Servlet.Server.Tests;
