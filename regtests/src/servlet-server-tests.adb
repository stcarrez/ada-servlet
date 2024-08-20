-----------------------------------------------------------------------
--  servlet-server-tests - Unit tests for server requests
--  Copyright (C) 2018, 2020 Stephane Carrez
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
with Util.Test_Caller;
with Util.Files;
with Servlet.Tests;
with Servlet.Core.Files;
with Servlet.Core.Measures;
with Servlet.Core.Tests;
with Servlet.Filters.Dump;
with Servlet.Requests.Mockup;
with Servlet.Responses.Mockup;

package body Servlet.Server.Tests is

   use Servlet.Tests;
   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Server");

   Except_Servlet : aliased Servlet.Core.Tests.Test_Servlet3;
   Upload         : aliased Servlet.Core.Tests.Test_Servlet2;
   Files          : aliased Servlet.Core.Files.File_Servlet;
   Dump           : aliased Servlet.Filters.Dump.Dump_Filter;
   Measures       : aliased Servlet.Core.Measures.Measure_Servlet;
   All_Servlet    : aliased Servlet.Core.Tests.Test_Servlet3;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Servlet.Server.Service",
                       Test_Service'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Service (GET)",
                       Test_Get_File'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Service (GET 404)",
                       Test_Get_404'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Service (POST)",
                       Test_Post_File_Error'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Service (POST)",
                       Test_Post_Content'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Service (GET measures)",
                       Test_Get_Measures'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Service (GET with exception)",
                       Test_Get_With_Exception'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Register_Application",
                       Test_Register_Remove_Application'Access);
      Caller.Add_Test (Suite, "Test Servlet.Server.Register_Application (all)",
                       Test_Register_Application'Access);
   end Add_Tests;

   --  ------------------------------
   --  Initialize the test.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
      use type Servlet.Core.Servlet_Registry_Access;
      App         : Servlet.Core.Servlet_Registry_Access;
   begin
      if Servlet.Tests.Get_Application = null then
         Servlet.Tests.Initialize (Util.Tests.Get_Properties);
         App := Servlet.Tests.Get_Application;
         App.Add_Servlet ("Except", Except_Servlet'Access);
         App.Add_Mapping ("*.exc", "Except");

         --  Register the servlets and filters
         App.Add_Servlet (Name => "files", Server => Files'Access);
         App.Add_Servlet (Name => "measures", Server => Measures'Access);
         App.Add_Filter (Name => "dump", Filter => Dump'Access);
         App.Add_Filter (Name => "measures",
                         Filter => Servlet.Filters.Filter'Class (Measures)'Access);

         App.Add_Servlet ("Upload", Upload'Access);
         App.Add_Mapping ("*.upload", "Upload");

         --  Define servlet mappings
         App.Add_Mapping (Name => "files", Pattern => "*.css");
         App.Add_Mapping (Name => "files", Pattern => "*.js");
         App.Add_Mapping (Name => "files", Pattern => "*.html");
         App.Add_Mapping (Name => "files", Pattern => "*.txt");
         App.Add_Mapping (Name => "files", Pattern => "*.png");
         App.Add_Mapping (Name => "files", Pattern => "*.jpg");
         App.Add_Mapping (Name => "files", Pattern => "*.gif");
         App.Add_Mapping (Name => "files", Pattern => "*.pdf");
         App.Add_Mapping (Name => "files", Pattern => "*.properties");
         App.Add_Mapping (Name => "files", Pattern => "*.xhtml");
         App.Add_Mapping (Name => "measures", Pattern => "stats.xml");

         App.Add_Filter_Mapping (Name => "measures", Pattern => "*");
         App.Add_Filter_Mapping (Name => "measures", Pattern => "/ajax/*");
         App.Add_Filter_Mapping (Name => "measures", Pattern => "*.html");
         App.Add_Filter_Mapping (Name => "measures", Pattern => "*.xhtml");
         App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
         App.Add_Filter_Mapping (Name => "dump", Pattern => "*.css");
         App.Add_Filter_Mapping (Name => "dump", Pattern => "/ajax/*");
      end if;
      Servlet.Tests.Get_Application.Start;
   end Set_Up;

   --  ------------------------------
   --  Test the Service procedure.
   --  ------------------------------
   procedure Test_Service (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Request.Set_Method (Method => "GET");
      Request.Set_Request_URI (URI => "tst", Split => True);
      Request.Set_Protocol (Protocol => "HTTP/1.1");
      Servlet.Tests.Get_Server.Service (Request, Reply);
      Assert_Equals (T, Servlet.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid response");
      Assert_Matches (T, ".*servlet.error.status_code.*404.*", Reply, "Invalid 404 page returned",
                      Status => Servlet.Responses.SC_NOT_FOUND);
   end Test_Service;

   --  ------------------------------
   --  Test a GET request on a static file served by the File_Servlet.
   --  ------------------------------
   procedure Test_Get_404 (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/file-does-not-exist.txt", "test-404.html");
      Assert_Equals (T, Servlet.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid response");
      Assert_Matches (T, ".*servlet.error.status_code.*404.*", Reply, "Invalid 404 page returned",
                      Status => Servlet.Responses.SC_NOT_FOUND);

      Do_Get (Request, Reply, "/file-does-not-exist.js", "test-404.html");
      Assert_Equals (T, Servlet.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid response");
      Assert_Matches (T, ".*servlet.error.status_code.*404.*", Reply, "Invalid 404 page returned",
                      Status => Servlet.Responses.SC_NOT_FOUND);
   end Test_Get_404;

   --  ------------------------------
   --  Test a GET request on a static file served by the File_Servlet.
   --  ------------------------------
   procedure Test_Get_File (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/tests/file.txt", "get-file.txt");
      Assert_Contains (T, "A plain text file.", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/plain", Reply, "Content-Type");

      Do_Get (Request, Reply, "/tests/file.html", "get-file-set.html");
      Assert_Matches (T, "<html></html>", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/html", Reply, "Content-Type");

      Do_Get (Request, Reply, "/tests/file.js", "get-file.js");
      Assert_Matches (T, "^\s*var n = 0;.*", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/javascript", Reply, "Content-Type");

      Do_Get (Request, Reply, "/tests/file.css", "get-file.css");
      Assert_Matches (T, "^\s*div { margin: 0 }.*", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/css", Reply, "Content-Type");

   end Test_Get_File;

   --  ------------------------------
   --  Test a GET request on the measure servlet
   --  ------------------------------
   procedure Test_Get_Measures (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/stats.xml", "stats.xml");

      --  We must get at least one measure value (assuming the Test_Get_File test
      --  was executed).
      Assert_Matches (T, "<time count=""\d+"" time=""\d+.\d+ [um]s"" title="".*""/>",
                      Reply, "Wrong content");
   end Test_Get_Measures;

   --  ------------------------------
   --  Test a POST on a file served by the File_Servlet.
   --  ------------------------------
   procedure Test_Post_File_Error (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Do_Post (Request, Reply, "/tests/file.css", "post-file.css");
      Assert_Header (T, "Content-Type", "text/html", Reply, "Content-Type",
                     Status => Servlet.Responses.SC_METHOD_NOT_ALLOWED);
   end Test_Post_File_Error;

   --  ------------------------------
   --  Test a POST with a part file to a test servlet.
   --  ------------------------------
   procedure Test_Post_Content (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Test_Path ("upload.txt");
      Request : Servlet.Requests.Mockup.Part_Request (1);
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Util.Files.Write_File (Path, "Some content");
      Request.Set_Part (Position => 1,
                        Name => "file.txt",
                        Path => Path,
                        Content_Type => "text/plain");
      Do_Post (Request, Reply, "/tests/file.upload", "post-file.upload");
      Assert_Header (T, "Content-Type", "text/plain", Reply, "Content-Type",
                     Status => Servlet.Responses.SC_OK);
   end Test_Post_Content;

   --  ------------------------------
   --  Test a GET request on servlet that raises an exception.
   --  ------------------------------
   procedure Test_Get_With_Exception (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Except_Servlet.Raise_Exception := True;
      Do_Get (Request, Reply, "/exception-raised.exc", "exception-raised.exc");
      Assert_Header (T, "Content-Type", "text/html", Reply, "Content-Type",
                     Status => Servlet.Responses.SC_INTERNAL_SERVER_ERROR);
      Assert_Matches (T, ".*CONSTRAINT_ERROR.*",
                      Reply, "No exception reported",
                      Status => Servlet.Responses.SC_INTERNAL_SERVER_ERROR);
   end Test_Get_With_Exception;

   --  ------------------------------
   --  Test a Register_Application and Remove_Application.
   --  ------------------------------
   procedure Test_Register_Remove_Application (T : in out Test) is
      App1 : aliased Servlet.Core.Servlet_Registry;
   begin
      Servlet.Tests.Get_Server.Register_Application ("my-app", App1'Unchecked_Access);
      T.Test_Get_File;
      for I in 1 .. 2 loop
         Servlet.Tests.Get_Server.Remove_Application (App1'Unchecked_Access);
         T.Test_Get_File;
      end loop;

   exception
      when others =>
         Servlet.Tests.Get_Server.Remove_Application (App1'Unchecked_Access);
         raise;

   end Test_Register_Remove_Application;

   --  ------------------------------
   --  Test a Register_Application and Remove_Application.
   --  ------------------------------
   procedure Test_Register_Application (T : in out Test) is
      App1    : aliased Servlet.Core.Servlet_Registry;
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      App1.Add_Servlet ("all", All_Servlet'Access);
      App1.Add_Mapping ("/", "all");
      App1.Add_Mapping ("/test", "all");
      Servlet.Tests.Get_Server.Register_Application ("", App1'Unchecked_Access);
      Servlet.Tests.Get_Server.Start;

      Request.Set_Method (Method => "GET");
      Request.Set_Request_URI (URI => "/test", Split => True);
      Request.Set_Protocol (Protocol => "HTTP/1.1");
      Servlet.Tests.Get_Server.Service (Request, Reply);
      Assert_Equals (T, Servlet.Responses.SC_OK, Reply.Get_Status, "Invalid response");

      T.Test_Get_File;

      Servlet.Tests.Get_Server.Remove_Application (App1'Unchecked_Access);
   end Test_Register_Application;

end Servlet.Server.Tests;
