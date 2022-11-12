-----------------------------------------------------------------------
--  Servlet tests - Servlet Tests Framework
--  Copyright (C) 2011, 2012, 2013, 2015, 2017, 2018, 2020, 2022 Stephane Carrez
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

with GNAT.Regpat;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Util.Files;

with Servlet.Streams;
with Servlet.Responses;
with Servlet.Responses.Tools;

with EL.Variables.Default;

package body Servlet.Tests is

   use Ada.Strings.Unbounded;
   use Util.Tests;

   type Container_Access is access Servlet.Server.Container;

   Server      : Container_Access;

   App_Created : Servlet.Core.Servlet_Registry_Access;
   App         : Servlet.Core.Servlet_Registry_Access;
   App_URI     : Unbounded_String;

   --  Save the response headers and content in a file
   procedure Save_Response (Name     : in String;
                            Response : in out Servlet.Responses.Mockup.Response'Class);

   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props        : in Util.Properties.Manager'Class;
                         Context_Path : in String := "/servlet-unit";
                         Registry     : in Servlet.Core.Servlet_Registry_Access := null) is
      use type Servlet.Core.Servlet_Registry_Access;
   begin
      if Registry /= null then
         App := Registry;
      else
         if App_Created = null then
            App_Created := new Servlet.Core.Servlet_Registry;
         end if;
         App := App_Created;
      end if;

      if not Props.Is_Empty then
         App.Set_Init_Parameters (Props);
      end if;
      App_URI := To_Unbounded_String (Context_Path);
      Server := new Servlet.Server.Container;
      Server.Register_Application (Context_Path, App.all'Access);
   end Initialize;

   --  ------------------------------
   --  Called when the testsuite execution has finished.
   --  ------------------------------
   procedure Finish (Status : in Util.XUnit.Status) is
      pragma Unreferenced (Status);

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Servlet.Server.Container,
                                        Name   => Container_Access);

   begin
      Free (Server);
   end Finish;

   --  ------------------------------
   --  Get the server
   --  ------------------------------
   function Get_Server return access Servlet.Server.Container is
   begin
      return Server;
   end Get_Server;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return Servlet.Core.Servlet_Registry_Access is
   begin
      return App;
   end Get_Application;

   --  ------------------------------
   --  Save the response headers and content in a file
   --  ------------------------------
   procedure Save_Response (Name     : in String;
                            Response : in out Servlet.Responses.Mockup.Response'Class) is
      use Servlet.Responses;

      Info     : constant String := Tools.To_String (Reply         => Response,
                                                     Html          => False,
                                                     Print_Headers => True);
      Path     : constant String := Util.Tests.Get_Test_Path (Name);
      Content  : Unbounded_String;
      Stream   : Servlet.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      Response.Read_Content (Content);
      Stream.Write (Content);

      Insert (Content, 1, Info);
      Util.Files.Write_File (Path, Content);
   end Save_Response;

   --  ------------------------------
   --  Simulate a raw request.  The URI and method must have been set on the Request object.
   --  ------------------------------
   procedure Do_Req (Request  : in out Servlet.Requests.Mockup.Request'Class;
                     Response : in out Servlet.Responses.Mockup.Response'Class) is
   begin
      --  For the purpose of writing tests, clear the buffer before invoking the service.
      Response.Clear;
      Server.Service (Request  => Request,
                      Response => Response);
   end Do_Req;

   --  ------------------------------
   --  Simulate a GET request on the given URI with the request parameters.
   --  Get the result in the response object.
   --  ------------------------------
   procedure Do_Get (Request  : in out Servlet.Requests.Mockup.Request'Class;
                     Response : in out Servlet.Responses.Mockup.Response'Class;
                     URI      : in String;
                     Save     : in String := "") is
   begin
      Request.Set_Method (Method => "GET");
      Request.Set_Request_URI (URI => To_String (App_URI) & URI, Split => True);
      Request.Set_Protocol (Protocol => "HTTP/1.1");
      Do_Req (Request, Response);

      if Save'Length > 0 then
         Save_Response (Save, Response);
      end if;
   end Do_Get;

   --  ------------------------------
   --  Simulate a POST request on the given URI with the request parameters.
   --  Get the result in the response object.
   --  ------------------------------
   procedure Do_Post (Request  : in out Servlet.Requests.Mockup.Request'Class;
                      Response : in out Servlet.Responses.Mockup.Response'Class;
                      URI      : in String;
                      Save     : in String := "") is
   begin
      Request.Set_Method (Method => "POST");
      Request.Set_Request_URI (URI => To_String (App_URI) & URI, Split => False);
      Request.Set_Protocol (Protocol => "HTTP/1.1");
      Do_Req (Request, Response);

      if Save'Length > 0 then
         Save_Response (Save, Response);
      end if;
   end Do_Post;

   --  ------------------------------
   --  Check that the response body contains the string
   --  ------------------------------
   procedure Assert_Contains (T       : in Util.Tests.Test'Class;
                              Value   : in String;
                              Reply   : in out Servlet.Responses.Mockup.Response;
                              Message : in String := "Test failed";
                              Source  : String := GNAT.Source_Info.File;
                              Line    : Natural := GNAT.Source_Info.Line) is
      Stream  : Servlet.Streams.Print_Stream := Reply.Get_Output_Stream;
      Content : Unbounded_String;
   begin
      Reply.Read_Content (Content);
      Stream.Write (Content);

      Assert_Equals (T, Servlet.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response", Source, Line);

      T.Assert (Condition => Index (Content, Value) > 0,
                Message   => Message & ": value '" & Value & "' not found",
                Source    => Source,
                Line      => Line);
   end Assert_Contains;

   --  ------------------------------
   --  Check that the response body matches the regular expression
   --  ------------------------------
   procedure Assert_Matches (T       : in Util.Tests.Test'Class;
                             Pattern : in String;
                             Reply   : in out Servlet.Responses.Mockup.Response;
                             Message : in String := "Test failed";
                             Status  : in Natural := Servlet.Responses.SC_OK;
                             Source  : String := GNAT.Source_Info.File;
                             Line    : Natural := GNAT.Source_Info.Line) is
      use GNAT.Regpat;

      Stream  : Servlet.Streams.Print_Stream := Reply.Get_Output_Stream;
      Content : Unbounded_String;
      Regexp  : constant Pattern_Matcher := Compile (Expression => Pattern,
                                                     Flags      => Multiple_Lines);
   begin
      Reply.Read_Content (Content);
      Stream.Write (Content);

      Assert_Equals (T, Status, Reply.Get_Status, "Invalid response", Source, Line);

      T.Assert (Condition => Match (Regexp, To_String (Content)),
                Message   => Message & ": does not match '" & Pattern & "'",
                Source    => Source,
                Line      => Line);
   end Assert_Matches;

   --  ------------------------------
   --  Check that the response contains the given header.
   --  ------------------------------
   procedure Assert_Header (T       : in Util.Tests.Test'Class;
                            Header  : in String;
                            Value   : in String;
                            Reply   : in out Servlet.Responses.Mockup.Response;
                            Message : in String := "Test failed";
                            Status  : in Natural := Servlet.Responses.SC_OK;
                            Source  : String := GNAT.Source_Info.File;
                            Line    : Natural := GNAT.Source_Info.Line) is
   begin
      Assert_Equals (T, Status, Reply.Get_Status,
                     "Invalid response status", Source, Line);
      T.Assert (Condition => Reply.Contains_Header (Header),
                Message   => Message & ": missing header '" & Header & "'",
                Source    => Source,
                Line      => Line);
      Assert_Equals (T, Value, Reply.Get_Header (Header), Message, Source, Line);
   end Assert_Header;

   --  ------------------------------
   --  Check that the response body is a redirect to the given URI.
   --  ------------------------------
   procedure Assert_Redirect (T       : in Util.Tests.Test'Class;
                              Value   : in String;
                              Reply   : in out Servlet.Responses.Mockup.Response;
                              Message : in String := "Test failed";
                              Source  : String := GNAT.Source_Info.File;
                              Line    : Natural := GNAT.Source_Info.Line) is
   begin
      Assert_Equals (T, Servlet.Responses.SC_MOVED_TEMPORARILY, Reply.Get_Status,
                     "Invalid response", Source, Line);

      Util.Tests.Assert_Equals (T, Value, Reply.Get_Header ("Location"),
                                Message & ": missing Location",
                                Source, Line);
   end Assert_Redirect;

   --  ------------------------------
   --  Cleanup the test instance.
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out EL_Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_Context'Class,
                                        EL.Contexts.Default.Default_Context_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Variables.Variable_Mapper'Class,
                                        EL.Variables.Variable_Mapper_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_ELResolver'Class,
                                        EL.Contexts.Default.Default_ELResolver_Access);
   begin
      Free (T.ELContext);
      Free (T.Variables);
      Free (T.Root_Resolver);
   end Tear_Down;

   --  ------------------------------
   --  Setup the test instance.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out EL_Test) is
   begin
      T.ELContext     := new EL.Contexts.Default.Default_Context;
      T.Root_Resolver := new EL.Contexts.Default.Default_ELResolver;
      T.Variables     := new EL.Variables.Default.Default_Variable_Mapper;
      T.ELContext.Set_Resolver (T.Root_Resolver.all'Access);
      T.ELContext.Set_Variable_Mapper (T.Variables.all'Access);
   end Set_Up;

end Servlet.Tests;
