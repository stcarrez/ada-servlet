-----------------------------------------------------------------------
--  servlet-rest-tests - Unit tests for Servlet.Rest and Servlet.Servlets.Rest
--  Copyright (C) 2016, 2017 Stephane Carrez
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

with Util.Log;
with Util.Test_Caller;
with Util.Measures;

with EL.Contexts.Default;

with Security.Permissions;
with Servlet.Requests.Mockup;
with Servlet.Responses.Mockup;
with Servlet.Servlets.Rest;
with Servlet.Rest.Definition;
with Servlet.Rest.Operation;

package body Servlet.Rest.Tests is

   package Caller is new Util.Test_Caller (Test, "Rest");

   package Test_Permission is
      new Security.Permissions.Definition ("test-permission");

   package API_Simple_Get is
     new Servlet.Rest.Operation (Handler    => Simple_Get'Access,
                             URI        => "/simple/:id");

   package API_Simple_List is
     new Servlet.Rest.Operation (Handler    => Simple_Get'Access,
                             URI        => "/simple");

   package API_Simple_Post is
     new Servlet.Rest.Operation (Handler    => Simple_Post'Access,
                             URI        => "/simple",
                             Method     => Servlet.Rest.POST);

   package API_Simple_Delete is
     new Servlet.Rest.Operation (Handler    => Simple_Delete'Access,
                             URI        => "/simple/:id",
                             Method     => Servlet.Rest.DELETE);

   package API_Simple_Put is
     new Servlet.Rest.Operation (Handler    => Simple_Put'Access,
                             URI        => "/simple/:id",
                             Method     => Servlet.Rest.PUT);

   package Test_API_Definition is
     new Servlet.Rest.Definition (Object_Type => Test_API,
                              URI         => "/test");

   package API_Create is
     new Test_API_Definition.Definition (Handler    => Create'Access,
                                         Method     => Servlet.Rest.POST,
                                         Pattern    => "",
                                         Permission => Test_Permission.Permission);

   package API_Update is
     new Test_API_Definition.Definition (Handler    => Update'Access,
                                         Method     => Servlet.Rest.PUT,
                                         Pattern    => ":id",
                                         Permission => 0);

   package API_Delete is
     new Test_API_Definition.Definition (Handler    => Delete'Access,
                                         Method     => Servlet.Rest.DELETE,
                                         Pattern    => ":id",
                                         Permission => 0);

   package API_List is
     new Test_API_Definition.Definition (Handler    => List'Access,
                                         Method     => Servlet.Rest.GET,
                                         Pattern    => "",
                                         Permission => 0);

   package API_Get is
     new Test_API_Definition.Definition (Handler    => List'Access,
                                         Method     => Servlet.Rest.GET,
                                         Pattern    => ":id",
                                         Permission => 0);

   procedure Simple_Get (Req    : in out Servlet.Rest.Request'Class;
                         Reply  : in out Servlet.Rest.Response'Class;
                         Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      List (Data, Req, Reply, Stream);
   end Simple_Get;

   procedure Simple_Put (Req    : in out Servlet.Rest.Request'Class;
                         Reply  : in out Servlet.Rest.Response'Class;
                         Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      Update (Data, Req, Reply, Stream);
   end Simple_Put;

   procedure Simple_Post (Req    : in out Servlet.Rest.Request'Class;
                          Reply  : in out Servlet.Rest.Response'Class;
                          Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      Create (Data, Req, Reply, Stream);
   end Simple_Post;

   procedure Simple_Delete (Req    : in out Servlet.Rest.Request'Class;
                            Reply  : in out Servlet.Rest.Response'Class;
                            Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      Delete (Data, Req, Reply, Stream);
   end Simple_Delete;

   procedure Create (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class) is
   begin
      Reply.Set_Status (Servlet.Responses.SC_CREATED);

      --  Servlet.Rest.Created (Reply, "23");
      Reply.Set_Header (Name  => "Location",
                        Value => "/test/23");
   end Create;

   procedure Update (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class) is
      Id : constant String := Req.Get_Path_Parameter (1);
   begin
      Reply.Set_Status (Servlet.Responses.SC_OK);
   end Update;

   procedure Delete (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class) is
   begin
      Reply.Set_Status (Servlet.Responses.SC_NO_CONTENT);
   end Delete;

   procedure List (Data   : in out Test_API;
                   Req    : in out Servlet.Rest.Request'Class;
                   Reply  : in out Servlet.Rest.Response'Class;
                   Stream : in out Servlet.Rest.Output_Stream'Class) is
   begin
      if Req.Get_Path_Parameter_Count = 0 then
         Stream.Start_Document;
         Stream.Start_Array ("list");
         for I in 1 .. 10 loop
            Stream.Start_Entity ("item");
            Stream.Write_Attribute ("id", I);
            Stream.Write_Attribute ("name", "Item " & Natural'Image (I));
            Stream.End_Entity ("item");
         end loop;
         Stream.End_Array ("list");
         Stream.End_Document;
      else
         declare
            Id : constant String := Req.Get_Path_Parameter (1);
         begin
            if Id = "100" then
               Reply.Set_Status (Servlet.Responses.SC_NOT_FOUND);
            elsif Id /= "44" then
               Reply.Set_Status (Servlet.Responses.SC_GONE);
            end if;
         end;
      end if;
   end List;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Servlet.Rest.POST API operation",
                       Test_Create'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.GET API operation",
                       Test_Get'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.PUT API operation",
                       Test_Update'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.DELETE API operation",
                       Test_Delete'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.TRACE API operation",
                       Test_Invalid'Access);
   end Add_Tests;

   procedure Benchmark (Ctx    : in Servlet.Servlets.Servlet_Registry;
                        Title  : in String;
                        Method : in String;
                        URI    : in String) is
      T : Util.Measures.Stamp;
   begin
      for I in 1 .. 1000 loop
         declare
            Request : Servlet.Requests.Mockup.Request;
            Reply   : Servlet.Responses.Mockup.Response;
            Dispatcher : constant Servlet.Servlets.Request_Dispatcher
              := Ctx.Get_Request_Dispatcher (Path => URI);
            Result : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Request.Set_Method (Method);
            Request.Set_Request_URI (URI);
            Servlet.Servlets.Forward (Dispatcher, Request, Reply);
         end;
      end loop;

      Util.Measures.Report (T, Title, 1000);
   end Benchmark;

   procedure Test_Operation (T      : in out Test;
                             Method : in String;
                             URI    : in String;
                             Status : in Natural) is
      use Servlet.Servlets;
      use Util.Tests;

      Ctx     : Servlet_Registry;
      S1      : aliased Servlet.Servlets.Rest.Rest_Servlet;
      EL_Ctx  : EL.Contexts.Default.Default_Context;
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Ctx.Add_Servlet ("API", S1'Unchecked_Access);
      Ctx.Add_Mapping (Name => "API", Pattern => "/simple/*");
      Ctx.Start;
      Ctx.Dump_Routes (Util.Log.INFO_LEVEL);
      Servlet.Rest.Register (Ctx, API_Simple_Get.Definition);
      Servlet.Rest.Register (Ctx, API_Simple_List.Definition);
      Servlet.Rest.Register (Ctx, API_Simple_Post.Definition);
      Servlet.Rest.Register (Ctx, API_Simple_Put.Definition);
      Servlet.Rest.Register (Ctx, API_Simple_Delete.Definition);
      Ctx.Dump_Routes (Util.Log.INFO_LEVEL);

      Test_API_Definition.Register (Registry  => Ctx,
                                    Name      => "API",
                                    ELContext => EL_Ctx);
      Request.Set_Method (Method);
      declare
         Dispatcher : constant Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => URI);
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Request.Set_Request_URI (URI);
         Forward (Dispatcher, Request, Reply);

         --  Check the response after the API method execution.
         Reply.Read_Content (Result);
         Assert_Equals (T, Status, Reply.Get_Status, "Invalid status for " & Method & ":" & URI);
      end;
      Benchmark (Ctx, Method & " " & URI, Method, URI);
   end Test_Operation;

   --  ------------------------------
   --  Test REST POST create operation
   --  ------------------------------
   procedure Test_Create (T : in out Test) is
   begin
      Test_Operation (T, "POST", "/test", Servlet.Responses.SC_CREATED);
      Test_Operation (T, "POST", "/simple", Servlet.Responses.SC_CREATED);
   end Test_Create;

   --  ------------------------------
   --  Test REST PUT update operation
   --  ------------------------------
   procedure Test_Update (T : in out Test) is
   begin
      Test_Operation (T, "PUT", "/test/44", Servlet.Responses.SC_OK);
      Test_Operation (T, "PUT", "/simple/44", Servlet.Responses.SC_OK);
   end Test_Update;

   --  ------------------------------
   --  Test REST GET operation
   --  ------------------------------
   procedure Test_Get (T : in out Test) is
   begin
      Test_Operation (T, "GET", "/test", Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/test/44", Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/test/100", Servlet.Responses.SC_NOT_FOUND);
      Test_Operation (T, "GET", "/simple", Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple/44", Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple/100", Servlet.Responses.SC_NOT_FOUND);
   end Test_Get;

   --  ------------------------------
   --  Test REST DELETE delete operation
   --  ------------------------------
   procedure Test_Delete (T : in out Test) is
   begin
      Test_Operation (T, "DELETE", "/test/44", Servlet.Responses.SC_NO_CONTENT);
      Test_Operation (T, "DELETE", "/simple/44", Servlet.Responses.SC_NO_CONTENT);
   end Test_Delete;

   --  ------------------------------
   --  Test REST operation on invalid operation.
   --  ------------------------------
   procedure Test_Invalid (T : in out Test) is
   begin
      Test_Operation (T, "TRACE", "/test/44", Servlet.Responses.SC_NOT_FOUND);
      Test_Operation (T, "TRACE", "/simple/44", Servlet.Responses.SC_NOT_FOUND);
   end Test_Invalid;

end Servlet.Rest.Tests;
