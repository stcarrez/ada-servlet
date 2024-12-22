-----------------------------------------------------------------------
--  Servlet testsuite - Ada Server Faces Test suite
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Sessions.Tests;
with Servlet.Core.Tests;
with Servlet.Security.Tests;
with Servlet.Requests.Tests;
with Servlet.Routes.Tests;
with Servlet.Server.Tests;
with Servlet.Rest.Tests;
with Servlet.Samples_Tests;

package body Servlet.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   package AWS_Samples_Tests is new Servlet.Samples_Tests (Prefix => "aws");
   package EWS_Samples_Tests is new Servlet.Samples_Tests (Prefix => "ews");

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      AWS_Samples_Tests.Add_Tests (Ret);
      EWS_Samples_Tests.Add_Tests (Ret);
      Servlet.Routes.Tests.Add_Tests (Ret);
      Servlet.Requests.Tests.Add_Tests (Ret);
      Servlet.Sessions.Tests.Add_Tests (Ret);
      Servlet.Core.Tests.Add_Tests (Ret);
      Servlet.Security.Tests.Add_Tests (Ret);
      Servlet.Server.Tests.Add_Tests (Ret);
      Servlet.Rest.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end Servlet.Testsuite;
