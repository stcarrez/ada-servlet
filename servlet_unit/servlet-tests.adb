-----------------------------------------------------------------------
--  Servlet tests - Servlet Tests Framework
--  Copyright (C) 2011, 2012, 2013, 2015, 2017 Stephane Carrez
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
--  with Servlet.Servlets.Faces;
with Servlet.Servlets.Files;
--  with Servlet.Servlets.Ajax;
with Servlet.Servlets.Measures;
with Servlet.Responses;
with Servlet.Responses.Tools;

with Servlet.Filters.Dump;
--  with Servlet.Contexts.Faces;
with EL.Variables.Default;

package body Servlet.Tests is

   use Ada.Strings.Unbounded;
   use Util.Tests;

   CONTEXT_PATH : constant String := "/servlet-nit";

   type Container_Access is access Servlet.Server.Container;

   Server      : Container_Access;

   App_Created : Servlet.Servlets.Servlet_Registry_Access;
   App         : Servlet.Servlets.Servlet_Registry_Access;
--     App_Created : Servlet.Applications.Main.Application_Access;
--     App         : Servlet.Applications.Main.Application_Access;
--     Faces       : aliased Servlet.Servlets.Faces.Faces_Servlet;
   Files       : aliased Servlet.Servlets.Files.File_Servlet;
--     Ajax        : aliased Servlet.Servlets.Ajax.Ajax_Servlet;
   Dump        : aliased Servlet.Filters.Dump.Dump_Filter;
   Measures    : aliased Servlet.Servlets.Measures.Measure_Servlet;

   --  Save the response headers and content in a file
   procedure Save_Response (Name     : in String;
                            Response : in out Servlet.Responses.Mockup.Response);

   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props       : in Util.Properties.Manager;
                         Registry    : in Servlet.Servlets.Servlet_Registry_Access := null) is
--                           Application : in Servlet.Applications.Main.Application_Access := null;
--                           Factory     : in out Servlet.Applications.Main.Application_Factory'Class) is
      use type Servlet.Servlets.Servlet_Registry_Access;

      C        : Util.Properties.Manager;
   begin
      if Registry /= null then
         App := Registry;
      else
         if App_Created = null then
            App_Created := new Servlet.Servlets.Servlet_Registry;
         end if;
         App := App_Created;
      end if;

      Server := new Servlet.Server.Container;
      Server.Register_Application (CONTEXT_PATH, App.all'Access);

      C.Copy (Props);
--        App.Initialize (C, Factory);
--        App.Register ("layoutMsg", "layout");
--        App.Set_Global ("contextPath", CONTEXT_PATH);

      --  Register the servlets and filters
--        App.Add_Servlet (Name => "faces", Server => Faces'Access);
      App.Add_Servlet (Name => "files", Server => Files'Access);
--        App.Add_Servlet (Name => "ajax", Server => Ajax'Access);
      App.Add_Servlet (Name => "measures", Server => Measures'Access);
      App.Add_Filter (Name => "dump", Filter => Dump'Access);
      App.Add_Filter (Name => "measures", Filter => Servlet.Filters.Filter'Class (Measures)'Access);

      --  Define servlet mappings
--        App.Add_Mapping (Name => "faces", Pattern => "*.html");
      App.Add_Mapping (Name => "files", Pattern => "*.css");
      App.Add_Mapping (Name => "files", Pattern => "*.js");
      App.Add_Mapping (Name => "files", Pattern => "*.properties");
      App.Add_Mapping (Name => "files", Pattern => "*.xhtml");
--        App.Add_Mapping (Name => "ajax", Pattern => "/ajax/*");
      App.Add_Mapping (Name => "measures", Pattern => "stats.xml");

      App.Add_Filter_Mapping (Name => "measures", Pattern => "*");
      App.Add_Filter_Mapping (Name => "measures", Pattern => "/ajax/*");
      App.Add_Filter_Mapping (Name => "measures", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "measures", Pattern => "*.xhtml");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.css");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "/ajax/*");
   end Initialize;

   --  ------------------------------
   --  Called when the testsuite execution has finished.
   --  ------------------------------
   procedure Finish (Status : in Util.XUnit.Status) is
      pragma Unreferenced (Status);

--        procedure Free is
--          new Ada.Unchecked_Deallocation (Object => Servlet.Applications.Main.Application'Class,
--                                          Name   => Servlet.Applications.Main.Application_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Servlet.Server.Container,
                                        Name   => Container_Access);

   begin
--        Free (App_Created);
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
--     function Get_Application return Servlet.Applications.Main.Application_Access is
--     begin
--        return App;
--     end Get_Application;

   --  ------------------------------
   --  Save the response headers and content in a file
   --  ------------------------------
   procedure Save_Response (Name     : in String;
                            Response : in out Servlet.Responses.Mockup.Response) is
      use Servlet.Responses;

      Info        : constant String := Tools.To_String (Reply         => Response,
                                                        Html          => False,
                                                        Print_Headers => True);
      Result_Path : constant String := Util.Tests.Get_Test_Path ("regtests/result");
      Content     : Unbounded_String;
      Stream      : Servlet.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      Response.Read_Content (Content);
      Stream.Write (Content);

      Insert (Content, 1, Info);
      Util.Files.Write_File (Result_Path & "/" & Name, Content);

   end Save_Response;

   --  ------------------------------
   --  Simulate a raw request.  The URI and method must have been set on the Request object.
   --  ------------------------------
   procedure Do_Req (Request  : in out Servlet.Requests.Mockup.Request;
                     Response : in out Servlet.Responses.Mockup.Response) is
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
   procedure Do_Get (Request  : in out Servlet.Requests.Mockup.Request;
                     Response : in out Servlet.Responses.Mockup.Response;
                     URI      : in String;
                     Save     : in String := "") is
   begin
      Request.Set_Method (Method => "GET");
      Request.Set_Request_URI (URI => CONTEXT_PATH & URI, Split => True);
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
   procedure Do_Post (Request  : in out Servlet.Requests.Mockup.Request;
                      Response : in out Servlet.Responses.Mockup.Response;
                      URI      : in String;
                      Save     : in String := "") is
   begin
      Request.Set_Method (Method => "POST");
      Request.Set_Request_URI (URI => CONTEXT_PATH & URI, Split => False);
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

      Assert_Equals (T, Servlet.Responses.SC_OK, Reply.Get_Status, "Invalid response", Source, Line);

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
--        Servlet.Contexts.Faces.Restore (null);
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