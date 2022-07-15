-----------------------------------------------------------------------
--  servlet-rest-tests - Unit tests for Servlet.Rest and Servlet.Core.Rest
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
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

with Util.Tests;

package Servlet.Rest.Tests is

   --  Test API with simple operations.
   procedure Simple_Get (Req    : in out Servlet.Rest.Request'Class;
                         Reply  : in out Servlet.Rest.Response'Class;
                         Stream : in out Servlet.Rest.Output_Stream'Class);
   procedure Simple_Put (Req    : in out Servlet.Rest.Request'Class;
                         Reply  : in out Servlet.Rest.Response'Class;
                         Stream : in out Servlet.Rest.Output_Stream'Class);
   procedure Simple_Post (Req    : in out Servlet.Rest.Request'Class;
                          Reply  : in out Servlet.Rest.Response'Class;
                          Stream : in out Servlet.Rest.Output_Stream'Class);
   procedure Simple_Delete (Req    : in out Servlet.Rest.Request'Class;
                            Reply  : in out Servlet.Rest.Response'Class;
                            Stream : in out Servlet.Rest.Output_Stream'Class);
   procedure Simple_Head (Req    : in out Servlet.Rest.Request'Class;
                          Reply  : in out Servlet.Rest.Response'Class;
                          Stream : in out Servlet.Rest.Output_Stream'Class);
   procedure Simple_Options (Req    : in out Servlet.Rest.Request'Class;
                             Reply  : in out Servlet.Rest.Response'Class;
                             Stream : in out Servlet.Rest.Output_Stream'Class);
   procedure Simple_Patch (Req    : in out Servlet.Rest.Request'Class;
                           Reply  : in out Servlet.Rest.Response'Class;
                           Stream : in out Servlet.Rest.Output_Stream'Class);

   --  Test API with an object created for each request.
   type Test_API is record
      N : Natural := 0;
   end record;

   procedure Create (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure Update (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure Delete (Data   : in out Test_API;
                     Req    : in out Servlet.Rest.Request'Class;
                     Reply  : in out Servlet.Rest.Response'Class;
                     Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure List (Data   : in out Test_API;
                   Req    : in out Servlet.Rest.Request'Class;
                   Reply  : in out Servlet.Rest.Response'Class;
                   Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure Head (Data   : in out Test_API;
                   Req    : in out Servlet.Rest.Request'Class;
                   Reply  : in out Servlet.Rest.Response'Class;
                   Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure Options (Data   : in out Test_API;
                      Req    : in out Servlet.Rest.Request'Class;
                      Reply  : in out Servlet.Rest.Response'Class;
                      Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure Patch (Data   : in out Test_API;
                    Req    : in out Servlet.Rest.Request'Class;
                    Reply  : in out Servlet.Rest.Response'Class;
                    Stream : in out Servlet.Rest.Output_Stream'Class);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test REST POST create operation
   procedure Test_Create (T : in out Test);

   --  Test REST GET operation
   procedure Test_Get (T : in out Test);

   --  Test REST PUT update operation
   procedure Test_Update (T : in out Test);

   --  Test REST DELETE delete operation
   procedure Test_Delete (T : in out Test);

   --  Test REST HEAD operation
   procedure Test_Head (T : in out Test);

   --  Test REST OPTIONS operation
   procedure Test_Options (T : in out Test);

   --  Test REST PATCH operation
   procedure Test_Patch (T : in out Test);

   --  Test REST operation on invalid operation.
   procedure Test_Invalid (T : in out Test);

   procedure Test_Operation (T      : in out Test;
                             Method : in String;
                             URI    : in String;
                             Status : in Natural);

   --  Test Get_Mime_Type and resolution to handle the Accept header.
   procedure Test_Get_Mime_Type (T : in out Test);

end Servlet.Rest.Tests;
