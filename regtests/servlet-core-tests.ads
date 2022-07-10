-----------------------------------------------------------------------
--  Servlets Tests - Unit tests for Servlet.Core
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2018, 2022 Stephane Carrez
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
with Util.Beans.Basic;
with Util.Beans.Objects;

package Servlet.Core.Tests is

   use Ada.Strings.Unbounded;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test_Servlet1 is new Servlet with record
      Add_Resource : Boolean := False;
   end record;

   overriding
   procedure Do_Get (Server   : in Test_Servlet1;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   type Test_Servlet2 is new Test_Servlet1 with null record;

   overriding
   procedure Do_Post (Server   : in Test_Servlet2;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   type Test_Servlet3 is new Servlet with record
      Raise_Exception : Boolean := False;
   end record;

   overriding
   procedure Do_Get (Server   : in Test_Servlet3;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   type Test is new Util.Tests.Test with record
      Writer    : Integer;
   end record;

   --  Test creation of session
   procedure Test_Create_Servlet (T : in out Test);

   --  Test add servlet
   procedure Test_Add_Servlet (T : in out Test);

   --  Test getting a resource path
   procedure Test_Get_Resource (T : in out Test);

   procedure Test_Request_Dispatcher (T : in out Test);

   --  Test mapping and servlet path on a request.
   procedure Test_Servlet_Path (T : in out Test);

   --  Test mapping and servlet path on a request.
   procedure Test_Filter_Mapping (T : in out Test);

   --  Test execution of filters
   procedure Test_Filter_Execution (T : in out Test);

   --  Test execution of filters on complex mapping.
   procedure Test_Complex_Filter_Execution (T : in out Test);

   --  Test execution of the cache control filter.
   procedure Test_Cache_Control_Filter (T : in out Test);

   --  Test reading XML configuration file.
   procedure Test_Read_Configuration (T : in out Test);

   --  Test the Get_Name_Dispatcher.
   procedure Test_Name_Dispatcher (T : in out Test);

   --  Check that the mapping for the given URI matches the server.
   procedure Check_Mapping (T      : in out Test;
                            Ctx    : in Servlet_Registry;
                            URI    : in String;
                            Server : in Servlet_Access;
                            Filter : in Natural := 0);

   --  Check that the request is done on the good servlet and with the correct servlet path
   --  and path info.
   procedure Check_Request (T            : in out Test;
                            Ctx          : in Servlet_Registry;
                            URI          : in String;
                            Servlet_Path : in String;
                            Path_Info    : in String);

   type Form_Bean is new Util.Beans.Basic.Bean with record
      Name       : Unbounded_String;
      Password   : Unbounded_String;
      Email      : Unbounded_String;
      Called     : Natural := 0;
      Gender     : Unbounded_String;
      Use_Flash  : Boolean := False;
      Def_Nav    : Boolean := False;
      Perm_Error : Boolean := False;
   end record;
   type Form_Bean_Access is access all Form_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Form_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Form_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

end Servlet.Core.Tests;
