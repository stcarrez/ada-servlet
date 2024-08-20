-----------------------------------------------------------------------
--  Servlet testsuite - Ada Server Faces Test suite
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2020 Stephane Carrez
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
with Servlet.Sessions.Tests;
with Servlet.Core.Tests;
with Servlet.Security.Tests;
with Servlet.Requests.Tests;
with Servlet.Routes.Tests;
with Servlet.Server.Tests;
with Servlet.Rest.Tests;

package body Servlet.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
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
