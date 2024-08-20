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
with AWS.Config.Set;
with AWS.Status;
with AWS.Response;

with Servlet.Requests.Web;
with Servlet.Responses.Web;
with Util.Http.Clients.AWS;

with Util.Log.Loggers;
package body Servlet.Server.Web is

   use Util.Log;
   use Ada.Strings.Unbounded;

   Log : constant Loggers.Logger := Loggers.Create ("Servlet.Server.Web");
   --  The logger

   function Server_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

   Server : AWS_Container_Access;

   overriding
   procedure Initialize (Instance : in out AWS_Container) is
   begin
      Instance.Conf := AWS.Config.Get_Current;
      AWS.Config.Set.Reuse_Address (O => Instance.Conf, Value => True);
      AWS.Config.Set.Upload_Directory (Instance.Conf, "upload");
      Server := Instance'Unchecked_Access;
   end Initialize;

   ----------------------
   --  Start the applications that have been registered.
   ----------------------
   overriding
   procedure Start (Server : in out AWS_Container) is
   begin
      Log.Info ("Starting server...");

      Container (Server).Start;

      AWS.Server.Start (Web_Server => Server.WS,
                        Config     => Server.Conf,
                        Callback   => Servlet.Server.Web.Server_Callback'Access);
   end Start;

   ----------------------
   --  Configure the AWS server.
   ----------------------
   procedure Configure (Server : in out AWS_Container;
                        Process : not null access procedure (Config : in out AWS.Config.Object)) is
   begin
      Process (Server.Conf);
   end Configure;

   ----------------------
   --  Configure the server before starting it.
   ----------------------
   overriding
   procedure Configure (Server : in out AWS_Container;
                        Config : in Configuration) is
   begin
      AWS.Config.Set.Server_Port (Server.Conf, Config.Listening_Port);
      AWS.Config.Set.Send_Buffer_Size (Server.Conf, Config.Buffer_Size);
      AWS.Config.Set.Max_Connection (Server.Conf, Config.Max_Connection);
      AWS.Config.Set.Reuse_Address (Server.Conf, Config.Reuse_Address);
      AWS.Config.Set.Accept_Queue_Size (Server.Conf, Config.Accept_Queue_Size);
      AWS.Config.Set.TCP_No_Delay (Server.Conf, Config.TCP_No_Delay);
      AWS.Config.Set.Upload_Size_Limit (Server.Conf, Config.Upload_Size_Limit);
      AWS.Config.Set.Upload_Directory (Server.Conf, To_String (Config.Upload_Directory));
      AWS.Config.Set.Input_Line_Size_Limit (Config.Input_Line_Size_Limit);
   end Configure;

   ----------------------
   --  Main server callback
   ----------------------
   function Server_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Req   : Servlet.Requests.Web.Request;
      Resp  : Servlet.Responses.Web.Response;
   begin
      Req.Set_Request (Request'Unrestricted_Access);

      Server.Service (Req, Resp);

      Resp.Build;
      return Resp.Get_Data;
   end Server_Callback;

begin
   Util.Http.Clients.AWS.Register;
end Servlet.Server.Web;
