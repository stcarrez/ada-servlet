-----------------------------------------------------------------------
--  servlet-routes-tests - Unit tests for Servlet.Routes
--  Copyright (C) 2015, 2020, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Util.Tests;
with Util.Beans.Objects;
with Servlet.Tests;

package Servlet.Routes.Tests is

   --  A test bean to verify the path parameter injection.
   type Test_Bean is new Util.Beans.Basic.Bean with record
      Id   : Ada.Strings.Unbounded.Unbounded_String;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Test_Bean_Access is access all Test_Bean;

   overriding
   function Get_Value (Bean : in Test_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   overriding
   procedure Set_Value (Bean  : in out Test_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   MAX_TEST_ROUTES : constant Positive := 100;

   type Test_Route_Type is new Route_Type with record
      Index : Natural := 0;
   end record;
   type Test_Route_Type_Access is access Test_Route_Type;
   type Test_Route_Array is array (1 .. MAX_TEST_ROUTES) of Route_Type_Ref;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Servlet.Tests.EL_Test with record
      Routes : Test_Route_Array;
      Bean   : Test_Bean_Access;
   end record;

   --  Setup the test instance.
   overriding
   procedure Set_Up (T : in out Test);

   --  Verify that the path matches the given route.
   procedure Verify_Route (T      : in out Test;
                           Router : in out Router_Type;
                           Path   : in String;
                           Index  : in Positive;
                           Bean   : in out Test_Bean'Class);

   --  Add the route associted with the path pattern.
   procedure Add_Route (T      : in out Test;
                        Router : in out Router_Type;
                        Path   : in String;
                        Index  : in Positive;
                        Bean   : in out Test_Bean'Class);

   --  Test the Add_Route with "/".
   procedure Test_Add_Route_With_Root_Path (T : in out Test);

   --  Test the Add_Route with simple fixed path components.
   --  Example: /list/index.html
   procedure Test_Add_Route_With_Path (T : in out Test);

   --  Test the Add_Route with extension mapping.
   --  Example: /list/*.html
   procedure Test_Add_Route_With_Ext (T : in out Test);

   --  Test the Add_Route with fixed path components and path parameters.
   --  Example: /users/:id/view.html
   procedure Test_Add_Route_With_Param (T : in out Test);

   --  Test the Add_Route with fixed path components and path parameters (Alternate syntax).
   --  Example: /users/{id}/view.html
   procedure Test_Add_Route_With_Param_Alt (T : in out Test);

   --  Test the Add_Route with fixed path components and EL path injection.
   --  Example: /users/#{user.id}/view.html
   procedure Test_Add_Route_With_EL (T : in out Test);

   --  Test the wildcard route /* with a path whose starting index is > 1.
   procedure Test_Wildcard_Route (T : in out Test);

   --  Test the Iterate over several paths.
   procedure Test_Iterate (T : in out Test);

end Servlet.Routes.Tests;
