-----------------------------------------------------------------------
--  servlet-server -- Servlet Server
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2016, 2020 Stephane Carrez
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
with Ada.Finalization;
with Ada.Strings.Unbounded;

with Servlet.Requests;
with Servlet.Responses;
with Servlet.Core;
package Servlet.Server is

   subtype Port_Number is Natural range 0 .. 65535;

   type Configuration is record
      Listening_Port        : Port_Number := 8080;
      Max_Connection        : Positive := 5;
      Buffer_Size           : Positive := 128 * 1024;
      Accept_Queue_Size     : Positive := 63;
      Upload_Size_Limit     : Positive := 16#500_000#;
      Input_Line_Size_Limit : Positive := 16#4000#;
      Reuse_Address         : Boolean := True;
      TCP_No_Delay          : Boolean := False;
      Upload_Directory      : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Container is tagged limited private;

   --  Register the application to serve requests
   procedure Register_Application (Server  : in out Container;
                                   URI     : in String;
                                   Context : in Core.Servlet_Registry_Access);

   --  Remove the application
   procedure Remove_Application (Server  : in out Container;
                                 Context : in Core.Servlet_Registry_Access);

   --  Configure the server before starting it.
   procedure Configure (Server : in out Container;
                        Config : in Configuration) is null;

   --  Start the applications that have been registered.
   procedure Start (Server : in out Container);

   --  Receives standard HTTP requests from the public service method and
   --  dispatches them to the Do_XXX methods defined in this class. This method
   --  is an HTTP-specific version of the Servlet.service(Request, Response)
   --  method. There's no need to override this method.
   procedure Service (Server   : in Container;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Get the current registry associated with the current request being processed
   --  by the current thread.  Returns null if there is no current request.
   function Current return Core.Servlet_Registry_Access;

   --  Set the current registry (for unit testing mostly).
   procedure Set_Context (Context : in Core.Servlet_Registry_Access);

   --  Give access to the current request and response object to the `Process`
   --  procedure.  If there is no current request for the thread, do nothing.
   procedure Update_Context (Process : not null access
                               procedure (Request  : in out Requests.Request'Class;
                                          Response : in out Responses.Response'Class));

   --  Iterate over the application which are registered.
   procedure Iterate (Server  : in Container;
                      Process : not null access
                        procedure (URI     : in String;
                                   Context : in Core.Servlet_Registry_Access));

private

   --  Binding to record the Servlet applications and bind them to URI prefixes.
   --  It is expected that the number of Servlet applications is small
   --  (1-10 per server).
   type Binding (Len : Natural) is record
      Context  : Core.Servlet_Registry_Access;
      Base_URI : String (1 .. Len);
   end record;
   type Binding_Access is access all Binding;

   type Binding_Array is array (Natural range <>) of Binding_Access;
   type Binding_Array_Access is access all Binding_Array;

   type Container is new Ada.Finalization.Limited_Controlled with record
      Nb_Bindings  : Natural := 0;
      Applications : Binding_Array_Access := null;
      Default      : Core.Servlet_Registry;
      Is_Started   : Boolean := False;
   end record;

   type Request_Context is record
      Application : Core.Servlet_Registry_Access;
      Request     : Requests.Request_Access;
      Response    : Responses.Response_Access;
   end record;

   --  Set the current registry.  This is called by `Service` once the
   --  registry is identified from the URI.
   procedure Set_Context (Context : in Request_Context);

   --  Release the storage.
   overriding
   procedure Finalize (Server : in out Container);

end Servlet.Server;
