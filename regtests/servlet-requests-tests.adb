-----------------------------------------------------------------------
--  servlet-requests-tests - Unit tests for requests
--  Copyright (C) 2012, 2013, 2015, 2018, 2022 Stephane Carrez
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

with Util.Test_Caller;
with Util.Log.Loggers;
with Servlet.Requests.Mockup;
with Servlet.Responses.Mockup;
package body Servlet.Requests.Tests is

   use type Util.Http.Headers.Quality_Type;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Servlet.Requests.Tests");

   package Caller is new Util.Test_Caller (Test, "Requests");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Servlet.Requests.Split_Header",
                       Test_Split_Header'Access);
      Caller.Add_Test (Suite, "Test Servlet.Requests.Accept_Locales",
                       Test_Accept_Locales'Access);
      Caller.Add_Test (Suite, "Test Servlet.Requests.Set_Attribute",
                       Test_Set_Attribute'Access);
      Caller.Add_Test (Suite, "Test Servlet.Requests.Get_Header",
                       Test_Headers'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Split_Header procedure.
   --  ------------------------------
   procedure Test_Split_Header (T : in out Test) is
      procedure Process_Text (Item    : in String;
                              Quality : in Quality_Type);

      Count : Natural := 0;

      procedure Process_Text (Item    : in String;
                              Quality : in Quality_Type) is
      begin
         T.Assert (Item = "text/plain" or else Item = "text/html" or else Item = "text/x-dvi"
                     or else Item = "text/x-c", "Invalid item: " & Item);
         T.Assert (Quality = 0.5 or else Quality = 0.8 or else Quality = 1.0,
                   "Invalid quality");
         if Item = "text/plain" then
            T.Assert (Quality = 0.5, "Invalid quality for " & Item);

         elsif Item = "text/x-dvi" or else Item = "text/html" then
            T.Assert (Quality = 0.8, "Invalid quality for " & Item);

         else
            T.Assert (Quality = 1.0, "Invalid quality for " & Item);
         end if;
         Count := Count + 1;
      end Process_Text;

   begin
      Split_Header ("text/plain; q=0.5, text/html,text/x-dvi; q=0.8, text/x-c",
                    Process_Text'Access);
      Util.Tests.Assert_Equals (T, 4, Count, "Invalid number of items");
   end Test_Split_Header;

   --  ------------------------------
   --  Test the Accept_Locales procedure.
   --  ------------------------------
   procedure Test_Accept_Locales (T : in out Test) is
      procedure Process_Locale (Locale : in Util.Locales.Locale;
                                Quality : in Quality_Type);

      Req : Servlet.Requests.Mockup.Request;

      Count : Natural := 0;

      procedure Process_Locale (Locale : in Util.Locales.Locale;
                                Quality : in Quality_Type) is
         pragma Unreferenced (Quality);

         Lang : constant String := Util.Locales.Get_Language (Locale);
      begin
         Log.Info ("Found locale: {0}", Util.Locales.To_String (Locale));

         T.Assert (Lang = "da" or else Lang = "en_GB" or else Lang = "en",
                   "Invalid lang: " & Lang);

         Count := Count + 1;
      end Process_Locale;

   begin
      Req.Accept_Locales (Process_Locale'Access);
      Util.Tests.Assert_Equals (T, 1, Count, "Invalid number of calls");

      Count := 0;
      Req.Set_Header ("Accept-Language", "da, en-gb;q=0.8, en;q=0.7");
      Req.Accept_Locales (Process_Locale'Access);

      Util.Tests.Assert_Equals (T, 3, Count, "Invalid number of calls");
   end Test_Accept_Locales;

   --  ------------------------------
   --  Test the Set_Attribute procedure.
   --  ------------------------------
   procedure Test_Set_Attribute (T : in out Test) is
      use Util.Beans.Objects;
      Req : Servlet.Requests.Mockup.Request;
   begin
      Req.Set_Attribute ("page", Util.Beans.Objects.To_Object (Integer (1)));
      Util.Tests.Assert_Equals (T, 1, Util.Beans.Objects.To_Integer (Req.Get_Attribute ("page")),
                                "Invalid page attribute");

      Req.Remove_Attribute ("page");
      T.Assert (Util.Beans.Objects.Is_Null (Req.Get_Attribute ("page")),
                "Attribute page is not null");

      Req.Set_Attribute ("page", Util.Beans.Objects.To_Object (Integer (1)));
      Req.Set_Attribute ("page", Util.Beans.Objects.Null_Object);
      T.Assert (Util.Beans.Objects.Is_Null (Req.Get_Attribute ("page")),
                "Attribute page is not null");
   end Test_Set_Attribute;

   --  ------------------------------
   --  Test the getting, inserting headers.
   --  ------------------------------
   procedure Test_Headers (T : in out Test) is
      Req   : Servlet.Requests.Mockup.Request;
      Reply : Servlet.Responses.Mockup.Response;
      D     : Ada.Calendar.Time;
      V     : Integer;
   begin
      Req.Set_Header ("Content-Type", "text/plain");
      Req.Set_Header ("Count", "23");
      Req.Set_Header ("If-Modified-Since", "Tue, 09 Jan 2018 22:55:08 GMT");

      Util.Tests.Assert_Equals (T, "text/plain", Req.Get_Header ("Content-Type"),
                                "Invalid date header: Content-Type");
      D := Req.Get_Date_Header ("If-Modified-Since");

      --  Check the integer header conversion.
      V := Req.Get_Int_Header ("Count");
      Util.Tests.Assert_Equals (T, 23, V, "Invalid integer header: Count");

      T.Assert (not Req.Is_Ajax_Request, "Is_Ajax_Request must be false");
      Req.Set_Header ("X-Requested-With", "none");
      T.Assert (Req.Is_Ajax_Request, "Is_Ajax_Request must be true");

      --  Header tests on the response.
      Reply.Set_Header ("Count", "230");
      Reply.Set_Int_Header ("Count-Len", 44);
      Reply.Add_Int_Header ("Length", 123);
      Reply.Set_Date_Header ("Date", D);
      Reply.Add_Date_Header ("Second-Date", D);

      --  Check the integer header conversion.
      Util.Tests.Assert_Equals (T, "230", Reply.Get_Header ("Count"),
                                "Invalid response integer header: Count");
      Util.Tests.Assert_Equals (T, "44", Reply.Get_Header ("Count-Len"),
                                "Invalid response integer header: Count-Len");
      Util.Tests.Assert_Equals (T, "123", Reply.Get_Header ("Length"),
                                "Invalid response integer header: Length");

      Util.Tests.Assert_Equals (T, "Tue, 09 Jan 2018 22:55:08 GMT", Reply.Get_Header ("Date"),
                                "Invalid response date header: Date");

      Util.Tests.Assert_Equals (T, "Tue, 09 Jan 2018 22:55:08 GMT",
                                Reply.Get_Header ("Second-Date"),
                                "Invalid response date header: Date");

      Reply.Set_Content_Length (123);
      Util.Tests.Assert_Equals (T, "123", Reply.Get_Header ("Content-Length"));

      Reply.Set_Content_Length (456);
      Util.Tests.Assert_Equals (T, "456", Reply.Get_Header ("Content-Length"));

      Reply.Send_Redirect (Location => "https://somewhere.com");
      Util.Tests.Assert_Equals (T, "https://somewhere.com", Reply.Get_Header ("Location"));
   end Test_Headers;

end Servlet.Requests.Tests;
