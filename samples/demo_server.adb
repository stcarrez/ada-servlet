-----------------------------------------------------------------------
--  demo_server -- Demo server for Ada Server Faces
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Ada.IO_Exceptions;
with ASF.Server.Web;
with ASF.Servlets;
with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Measures;
with ASF.Filters.Dump;
with ASF.Beans;
with ASF.Applications;
with ASF.Applications.Main;
with ASF.Applications.Main.Configs;
with Security.Openid;
with Security.Openid.Servlets;

with Util.Beans.Objects;
with Util.Log.Loggers;

with Countries;
with Volume;
with Messages;
with Images;
with Users;
procedure Demo_Server is

   CONTEXT_PATH : constant String := "/demo";
   CONFIG_PATH  : constant String := "samples.properties";

   Log          : Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Openid");

   Factory      : ASF.Applications.Main.Application_Factory;
   C            : ASF.Applications.Config;

   --  Application servlets.
   App          : aliased ASF.Applications.Main.Application;
   Faces        : aliased ASF.Servlets.Faces.Faces_Servlet;
   Files        : aliased ASF.Servlets.Files.File_Servlet;
   Auth         : aliased Security.Openid.Servlets.Request_Auth_Servlet;
   Verify_Auth  : aliased Security.Openid.Servlets.Verify_Auth_Servlet;
   Perf         : aliased ASF.Servlets.Measures.Measure_Servlet;

   --  Debug filters.
   Dump         : aliased ASF.Filters.Dump.Dump_Filter;

   Bean         : aliased Volume.Compute_Bean;
   Conv         : aliased Volume.Float_Converter;
   None         : ASF.Beans.Parameter_Bean_Ref.Ref;

   --  Web application server
   WS           : ASF.Server.Web.AWS_Container;
begin
   C.Set (ASF.Applications.VIEW_EXT, ".html");
   C.Set (ASF.Applications.VIEW_DIR, "samples/web");
   C.Set ("web.dir", "samples/web");
   begin
      C.Load_Properties (CONFIG_PATH);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);

   end;
   C.Set ("contextPath", CONTEXT_PATH);
   App.Initialize (C, Factory);
   App.Set_Global ("contextPath", CONTEXT_PATH);
   App.Set_Global ("compute",
                   Util.Beans.Objects.To_Object (Bean'Unchecked_Access,
                                                 Util.Beans.Objects.STATIC));

   App.Set_Global ("countries", Util.Beans.Objects.To_Object (Countries.Create_Country_List));
   App.Set_Global ("user",
                   Util.Beans.Objects.To_Object (Users.User'Access, Util.Beans.Objects.STATIC));

   --  Declare a global bean to identify this sample from within the XHTML files.
   App.Set_Global ("sampleName", "volume");

   --  Register the servlets and filters
   App.Add_Servlet (Name => "faces", Server => Faces'Unchecked_Access);
   App.Add_Servlet (Name => "files", Server => Files'Unchecked_Access);
   App.Add_Filter (Name => "dump", Filter => Dump'Unchecked_Access);
   App.Add_Servlet (Name => "perf", Server => Perf'Unchecked_Access);
   App.Add_Filter (Name => "perf", Filter => Perf'Unchecked_Access);
   App.Add_Servlet (Name => "auth", Server => Auth'Unchecked_Access);
   App.Add_Servlet (Name => "verify-auth", Server => Verify_Auth'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "faces", Pattern => "*.html");
   App.Add_Mapping (Name => "files", Pattern => "*.css");
   App.Add_Mapping (Name => "files", Pattern => "*.js");
   App.Add_Mapping (Name => "files", Pattern => "*.png");
   App.Add_Mapping (Name => "verify-auth", Pattern => "/auth/verify");
   App.Add_Mapping (Name => "auth", Pattern => "/auth/auth/*");
   App.Add_Mapping (Name => "perf", Pattern => "/statistics.xml");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.js");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "/auth/verify");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "/auth/verify");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "/auth/auth/*");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "/auth/auth/*");

   App.Add_Converter (Name => "float", Converter => Conv'Unchecked_Access);

   App.Register_Class (Name => "Image_Bean", Handler => Images.Create_Image_Bean'Access);
   App.Register_Class (Name => "Message_Bean", Handler => Messages.Create_Message_Bean'Access);
   App.Register_Class (Name => "Message_List", Handler => Messages.Create_Message_List'Access);
   App.Register (Name => "message", Class => "Message_Bean", Params => None);
   App.Register (Name => "messages", Class => "Message_List", Params => None);
   App.Register (Name => "image", Class => "Image_Bean", Params => None);

   ASF.Applications.Main.Configs.Read_Configuration (App, "samples/web/WEB-INF/web.xml");
   WS.Register_Application (CONTEXT_PATH, App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:8080/demo/compute.html");
   WS.Start;

   delay 6000.0;

end Demo_Server;