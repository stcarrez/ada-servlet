-----------------------------------------------------------------------
--  servlet_harness -- Ada Servlet unit tests
--  Copyright (C) 2009, 2010, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Servlet.Tests;
with Servlet.Testsuite;
procedure Servlet_Harness is

   procedure Harness is new Util.Tests.Harness (Suite  => Servlet.Testsuite.Suite,
                                                Finish => Servlet.Tests.Finish);

begin
   Harness ("servlet-tests.xml");
end Servlet_Harness;
