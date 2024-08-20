-----------------------------------------------------------------------
--  servlet-server -- Servlet Server for AWS
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2019, 2020, 2021, 2022 Stephane Carrez
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
with GNAT.Sockets;
with EWS.Server; use EWS.Server;
with EWS.Dynamic; use EWS;
with EWS.HTTP;

with Servlet.Requests.EWS;
with Servlet.Responses.EWS;

with Util.Log.Loggers;
package body Servlet.Server.EWS is

   use Util.Log;

   function Handler (Request : in HTTP.Request_P) return Dynamic.Dynamic_Response'Class;

   Log : constant Loggers.Logger := Loggers.Create ("Servlet.Server.EWS");

   Server : EWS_Container_Access;

   function Handler (Request : in HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      Req   : Servlet.Requests.EWS.Request (Request);
      Reply : Servlet.Responses.EWS.Response (Request);
   begin
      Server.Service (Req, Reply);

      Reply.Build;
      return Reply.Get_Data;
   end Handler;

   overriding
   procedure Initialize (Instance : in out EWS_Container) is
   begin
      Server := Instance'Unchecked_Access;
   end Initialize;

   ----------------------
   --  Start the applications that have been registered.
   ----------------------
   overriding
   procedure Start (Server : in out EWS_Container) is
   begin
      Log.Info ("Starting server...");

      Container (Server).Start;

      Dynamic.Register_Default (Handler'Access);
      Serve (Using_Port => GNAT.Sockets.Port_Type (Server.Conf.Listening_Port),
             With_Stack => 1_000_000,
             Tracing => True);
   end Start;

   ----------------------
   --  Configure the server before starting it.
   ----------------------
   overriding
   procedure Configure (Server : in out EWS_Container;
                        Config : in Configuration) is
   begin
      Server.Conf := Config;
   end Configure;

end Servlet.Server.EWS;
