-----------------------------------------------------------------------
--  servlet-rest-tests - Unit tests for Servlet.Rest and Servlet.Core.Rest
--  Copyright (C) 2016, 2017, 2020, 2022, 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Util.Log;
with Util.Test_Caller;
with Util.Http.Headers;
with Util.Http.Mimes;

with Servlet.Requests.Mockup;
with Servlet.Responses.Mockup;
with Servlet.Core.Rest;
with Servlet.Rest.Operation;
with Servlet.Rest.Test_APIs;
package body Servlet.Rest.Tests is

   package Caller is new Util.Test_Caller (Test, "Rest");

   package Tests_JSON is
     new Test_APIs (NAME => "JSON",
                    Default_Streams => [Streams.Dynamic.JSON => True,
                                        others => False]);
   package Tests_XML is
     new Test_APIs (NAME => "XML",
                    Default_Streams => [Streams.Dynamic.XML => True,
                                        others => False]);
   package Tests_JSON_XML is
     new Test_APIs (NAME => "JSON+XML",
                    Default_Streams => [Streams.Dynamic.JSON => True,
                                        Streams.Dynamic.XML  => True,
                                        others => False]);
   package Tests_Dynamic is
     new Test_APIs (NAME => "Dynamic",
                    Default_Streams => [Streams.Dynamic.DYNAMIC => True,
                                        others => False]);

   Default_Mimes   : aliased constant Mime_List :=
     [1 => Util.Http.Mimes.Json'Access,
      2 => Util.Http.Mimes.Xml'Access,
      3 => Util.Http.Mimes.Jpg'Access];
   Default_Streams : constant Servlet.Rest.Stream_Modes := [Streams.Dynamic.DYNAMIC => True,
                                                            others => False];

   package API_Simple_Get is
     new Servlet.Rest.Operation (Handler    => Simple_Get'Access,
                                 URI        => "/simple/:id",
                                 Mimes      => Default_Mimes'Access,
                                 Streams    => Default_Streams);

   package API_Simple_List is
     new Servlet.Rest.Operation (Handler    => Simple_Get'Access,
                                 URI        => "/simple",
                                 Mimes      => Default_Mimes'Access,
                                 Streams    => Default_Streams);

   package API_Simple_Post is
     new Servlet.Rest.Operation (Handler    => Simple_Post'Access,
                                 URI        => "/simple",
                                 Method     => Servlet.Rest.POST,
                                 Mimes      => Default_Mimes'Access,
                                 Streams    => Default_Streams);

   package API_Simple_Delete is
     new Servlet.Rest.Operation (Handler    => Simple_Delete'Access,
                                 URI        => "/simple/:id",
                                 Method     => Servlet.Rest.DELETE,
                                 Mimes      => Default_Mimes'Access,
                                 Streams    => Default_Streams);

   package API_Simple_Put is
     new Servlet.Rest.Operation (Handler    => Simple_Put'Access,
                                 URI        => "/simple/:id",
                                 Method     => Servlet.Rest.PUT,
                                 Mimes      => Default_Mimes'Access,
                                 Streams    => Default_Streams);

   package API_Simple_Head is
      new Servlet.Rest.Operation (Handler    => Simple_Head'Access,
                                  URI        => "/simple/:id",
                                  Method     => Servlet.Rest.HEAD,
                                 Mimes      => Default_Mimes'Access,
                                  Streams    => Default_Streams);

   package API_Simple_Options is
      new Servlet.Rest.Operation (Handler    => Simple_Options'Access,
                                  URI        => "/simple/:id",
                                  Method     => Servlet.Rest.OPTIONS,
                                  Mimes      => Default_Mimes'Access,
                                  Streams    => Default_Streams);

   package API_Simple_Patch is
      new Servlet.Rest.Operation (Handler    => Simple_Patch'Access,
                                  URI        => "/simple/:id",
                                  Method     => Servlet.Rest.PATCH,
                                  Mimes      => Default_Mimes'Access,
                                  Streams    => Default_Streams);

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

   procedure Simple_Head (Req    : in out Servlet.Rest.Request'Class;
                          Reply  : in out Servlet.Rest.Response'Class;
                          Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      Head (Data, Req, Reply, Stream);
   end Simple_Head;

   procedure Simple_Options (Req    : in out Servlet.Rest.Request'Class;
                             Reply  : in out Servlet.Rest.Response'Class;
                             Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      Options (Data, Req, Reply, Stream);
   end Simple_Options;

   procedure Simple_Patch (Req    : in out Servlet.Rest.Request'Class;
                           Reply  : in out Servlet.Rest.Response'Class;
                           Stream : in out Servlet.Rest.Output_Stream'Class) is
      Data : Test_API;
   begin
      Patch (Data, Req, Reply, Stream);
   end Simple_Patch;

   procedure Create (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class) is
      pragma Unreferenced (Data, Req, Stream);
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
      pragma Unreferenced (Data, Stream);

      Id : constant String := Req.Get_Path_Parameter (1);
   begin
      if Id'Length > 0 then
         Reply.Set_Status (Servlet.Responses.SC_OK);
      else
         Reply.Set_Status (Servlet.Responses.SC_NOT_FOUND);
      end if;
   end Update;

   procedure Delete (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class) is
      pragma Unreferenced (Data, Req, Stream);
   begin
      Reply.Set_Status (Servlet.Responses.SC_NO_CONTENT);
   end Delete;

   procedure Head (Data   : in out Test_API;
                   Req    : in out Servlet.Rest.Request'Class;
                   Reply  : in out Servlet.Rest.Response'Class;
                   Stream : in out Servlet.Rest.Output_Stream'Class) is
      pragma Unreferenced (Data, Req, Stream);
   begin
      Reply.Set_Status (Servlet.Responses.SC_GONE);
   end Head;

   procedure Patch (Data   : in out Test_API;
                    Req    : in out Servlet.Rest.Request'Class;
                    Reply  : in out Servlet.Rest.Response'Class;
                    Stream : in out Servlet.Rest.Output_Stream'Class) is
      pragma Unreferenced (Data, Req, Stream);
   begin
      Reply.Set_Status (Servlet.Responses.SC_ACCEPTED);
   end Patch;

   procedure List (Data   : in out Test_API;
                   Req    : in out Servlet.Rest.Request'Class;
                   Reply  : in out Servlet.Rest.Response'Class;
                   Stream : in out Servlet.Rest.Output_Stream'Class) is
      pragma Unreferenced (Data);
   begin
      if Req.Get_Path_Parameter_Count = 0 then
         Servlet.Rest.Choose_Content_Type (Req, Reply, Stream);
         if Reply.Get_Content_Type = "" then
            Reply.Set_Content_Type (Util.Http.Mimes.Text);
         end if;
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
               Servlet.Rest.Set_Content_Type (Reply, Util.Http.Mimes.Text, Stream);
               Reply.Set_Status (Servlet.Responses.SC_NOT_FOUND);
               Stream.Write ("Document not found");
            elsif Id /= "44" then
               Reply.Set_Status (Servlet.Responses.SC_GONE);
            end if;
         end;
      end if;
   end List;

   procedure Options (Data   : in out Test_API;
                      Req    : in out Servlet.Rest.Request'Class;
                      Reply  : in out Servlet.Rest.Response'Class;
                      Stream : in out Servlet.Rest.Output_Stream'Class) is
      pragma Unreferenced (Data, Req, Stream);
   begin
      Reply.Set_Status (Servlet.Responses.SC_OK);

      Reply.Set_Header (Name  => "Allow",
                        Value => "OPTIONS, GET, POST, PUT, DELETE, PATCH");
   end Options;

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
      Caller.Add_Test (Suite, "Test Servlet.Rest.HEAD API operation",
                       Test_Head'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.TRACE API operation",
                       Test_Invalid'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.OPTIONS API operation",
                       Test_Options'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.PATCH API operation",
                       Test_Patch'Access);
      Caller.Add_Test (Suite, "Test Servlet.Rest.Get_Mime_Type",
                       Test_Get_Mime_Type'Access);
      Tests_JSON.Add_Tests (Suite);
      Tests_XML.Add_Tests (Suite);
      Tests_JSON_XML.Add_Tests (Suite);
      Tests_Dynamic.Add_Tests (Suite);
   end Add_Tests;

   procedure Test_Operation (T      : in out Test;
                             Method : in String;
                             URI    : in String;
                             Accept_Header : in String;
                             Status : in Natural) is
      use Servlet.Core;
      use Util.Tests;

      Ctx     : Servlet_Registry;
      S1      : aliased Servlet.Core.Rest.Rest_Servlet;
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
      Servlet.Rest.Register (Ctx, API_Simple_Head.Definition);
      Servlet.Rest.Register (Ctx, API_Simple_Options.Definition);
      Servlet.Rest.Register (Ctx, API_Simple_Patch.Definition);
      Ctx.Dump_Routes (Util.Log.INFO_LEVEL);

      Request.Set_Method (Method);
      declare
         Dispatcher : constant Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => URI);
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Request.Set_Request_URI (URI);
         if Accept_Header'Length > 0 then
            Request.Set_Header ("Accept", Accept_Header);
         end if;
         Forward (Dispatcher, Request, Reply);

         --  Check the response after the API method execution.
         Reply.Read_Content (Result);
         Assert_Equals (T, Status, Reply.Get_Status, "Invalid status for " & Method & ":" & URI);

         if Reply.Get_Status = 200 and then Accept_Header'Length > 0 then
            Assert_Equals (T, Accept_Header, Reply.Get_Content_Type, "Invalid Content-Type");
         end if;
      end;
   end Test_Operation;

   --  ------------------------------
   --  Test REST POST create operation
   --  ------------------------------
   procedure Test_Create (T : in out Test) is
   begin
      Test_Operation (T, "POST", "/simple",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_CREATED);
      Test_Operation (T, "POST", "/simple",
                      Util.Http.Mimes.Xml, Servlet.Responses.SC_CREATED);
   end Test_Create;

   --  ------------------------------
   --  Test REST PUT update operation
   --  ------------------------------
   procedure Test_Update (T : in out Test) is
   begin
      Test_Operation (T, "PUT", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_OK);
   end Test_Update;

   --  ------------------------------
   --  Test REST GET operation
   --  ------------------------------
   procedure Test_Get (T : in out Test) is
   begin
      Test_Operation (T, "GET", "/simple",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple",
                      Util.Http.Mimes.Xml, Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple",
                      Util.Http.Mimes.Text, Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple/44",
                      Util.Http.Mimes.Xml, Servlet.Responses.SC_OK);
      Test_Operation (T, "GET", "/simple/100",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_NOT_FOUND);
   end Test_Get;

   --  ------------------------------
   --  Test REST DELETE delete operation
   --  ------------------------------
   procedure Test_Delete (T : in out Test) is
   begin
      Test_Operation (T, "DELETE", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_NO_CONTENT);
   end Test_Delete;

   --  ------------------------------
   --  Test REST HEAD operation
   --  ------------------------------
   procedure Test_Head (T : in out Test) is
   begin
      Test_Operation (T, "HEAD", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_GONE);
   end Test_Head;

   --  ------------------------------
   --  Test REST OPTIONS operation
   --  ------------------------------
   procedure Test_Options (T : in out Test) is
   begin
      Test_Operation (T, "OPTIONS", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_OK);
   end Test_Options;

   --  ------------------------------
   --  Test REST PATCH operation
   --  ------------------------------
   procedure Test_Patch (T : in out Test) is
   begin
      Test_Operation (T, "PATCH", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_ACCEPTED);
   end Test_Patch;

   --  ------------------------------
   --  Test REST operation on invalid operation.
   --  ------------------------------
   procedure Test_Invalid (T : in out Test) is
   begin
      Test_Operation (T, "TRACE", "/simple/44",
                      Util.Http.Mimes.Json, Servlet.Responses.SC_NOT_FOUND);
   end Test_Invalid;

   --  ------------------------------
   --  Test Get_Mime_Type and resolution to handle the Accept header.
   --  ------------------------------
   procedure Test_Get_Mime_Type (T : in out Test) is
      use Util.Tests;

      Request : Servlet.Requests.Mockup.Request;
      Mime    : Mime_Access;
   begin
      Request.Set_Header (Util.Http.Headers.Accept_Header, "application/json");
      Mime := API_Simple_Get.Definition.Get_Mime_Type (Request);
      T.Assert (Mime /= null, "No matching mime type");
      Assert_Equals (T, Util.Http.Mimes.Json, Mime.all, "Invalid matching mime type");

      Request.Set_Header (Util.Http.Headers.Accept_Header, "application/xml");
      Mime := API_Simple_Get.Definition.Get_Mime_Type (Request);
      T.Assert (Mime /= null, "No matching mime type");
      Assert_Equals (T, Util.Http.Mimes.Xml, Mime.all, "Invalid matching mime type");

      Request.Set_Header (Util.Http.Headers.Accept_Header, "application/*");
      Mime := API_Simple_Get.Definition.Get_Mime_Type (Request);
      T.Assert (Mime /= null, "No matching mime type");
      Assert_Equals (T, Util.Http.Mimes.Json, Mime.all, "Invalid matching mime type");

   end Test_Get_Mime_Type;

end Servlet.Rest.Tests;
