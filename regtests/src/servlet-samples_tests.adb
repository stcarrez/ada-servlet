-----------------------------------------------------------------------
--  servlet-samples_tests - Tests the samples
--  Copyright (C) 2024 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Characters.Handling;
with Util.Test_Caller;
with Util.Http.Clients.AWS;

package body Servlet.Samples_Tests is

   NAME : constant String := Ada.Characters.Handling.To_Upper (Prefix);

   package Volume_Caller is new Util.Test_Caller (Volume_Test, "Samples.Volume." & NAME);
   package Upload_Caller is new Util.Test_Caller (Upload_Test, "Samples.Upload." & NAME);
   package Api_Caller is new Util.Test_Caller (Api_Test, "Samples.Rest." & NAME);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
      Label : constant String := "(" & Ada.Characters.Handling.To_Upper (Prefix) & ")";
   begin
      Util.Http.Clients.AWS.Register;
      Volume_Caller.Add_Test (Suite, "Test Servlet.Samples.Volume " & Label,
                              Test_Volume'Access);
      Upload_Caller.Add_Test (Suite, "Test Servlet.Samples.Upload " & Label,
                              Test_Upload'Access);
      Api_Caller.Add_Test (Suite, "Test Servlet.Samples.REST " & Label,
                           Test_Monitor'Access);
   end Add_Tests;

   procedure Test_Volume (T : in out Volume_Test) is
      URI    : constant String := Server_Volume_Tests.Get_URI;
      Client : Util.Http.Clients.Client;
      Reply  : Util.Http.Clients.Response;
      Data   : constant String := "height=2&radius=3";
   begin
      Client.Get (URI & "/volume/compute.html", Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");
      declare
         Content : constant String := Reply.Get_Body;
      begin
         Util.Tests.Assert_Matches (T, ".*<h1>Compute the volume of a cylinder</h1>.*",
                                    Content, "Invalid response");
      end;

      Client.Set_Header ("Content-Type", "application/x-www-form-urlencoded");
      Client.Post (URI & "/volume/compute.html", Data, Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");
      declare
         Content : constant String := Reply.Get_Body;
      begin
         Util.Tests.Assert_Matches (T, ".*The cylinder volume is:.*5.65487E\+01.*",
                                    Content, "Invalid response");
      end;
   end Test_Volume;

   procedure Test_Upload (T : in out Upload_Test) is
      URI    : constant String := Server_Upload_Tests.Get_URI;
      Client : Util.Http.Clients.Client;
      Reply  : Util.Http.Clients.Response;
      Boundary : constant String := "---------------------------615323372351087358832259188";
      Newline  : constant String := ASCII.CR & ASCII.LF;
   begin
      Client.Get (URI & "/upload/upload.html", Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");
      declare
         Content : constant String := Reply.Get_Body;
      begin
         Util.Tests.Assert_Matches (T, ".*<h1>Upload files to identify them</h1>.*",
                                    Content, "Invalid response");
      end;

      Client.Set_Header ("Content-Type", "multipart/form-data; boundary=" & Boundary);
      Client.Post (URI & "/upload/upload.html",
                   "--" & Boundary & Newline
                   & "Content-Disposition: form-data; name=""file1""; filename=""fuse.log"""
                   & Newline
                   & "Content-Type: text/x-log" & Newline & Newline
                   & "content" & Newline & Newline
                   & "--" & Boundary & "--" & Newline, Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");
      declare
         Content : constant String := Reply.Get_Body;
      begin
         Util.Tests.Assert_Matches (T, ".*Name: fuse.log.*",
                                    Content, "Invalid response");
      end;
   end Test_Upload;

   procedure Test_Monitor (T : in out Api_Test) is
      URI    : constant String := Server_Api_Tests.Get_URI;
      Client : Util.Http.Clients.Client;
      Reply  : Util.Http.Clients.Response;
   begin
      Client.Put (URI & "/monitor/api/monitor/1/configure?value=0.2", "", Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");

      for I in 1 .. 10 loop
         Client.Put (URI & "/monitor/api/monitor/1?value=2", "", Reply);
         Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                   "Invalid response status");
         delay 0.01;
      end loop;

      Client.Put (URI & "/monitor/api/monitor/1?value=3", "", Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");

      Client.Get (URI & "/monitor/api/monitor/1", Reply);
      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");

      Util.Tests.Assert_Equals (T, Util.Http.SC_OK, Reply.Get_Status,
                                "Invalid response status");
      declare
         Content : constant String := Reply.Get_Body;
      begin
         Util.Tests.Assert_Matches (T, ".*values.*0, 2.*",
                                    Content, "Invalid response");
      end;
   end Test_Monitor;

end Servlet.Samples_Tests;
