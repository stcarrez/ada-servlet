-----------------------------------------------------------------------
--  servlet-samples_servers - Tests the samples
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Processes;
with Util.Log.Loggers;
with Util.Strings.Vectors;

package body Servlet.Samples_Servers is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Servlet.Samples_Tests");

   Server : Util.Processes.Process;

   function Get_URI return String is
   begin
      return "http://localhost:" & Util.Strings.Image (Port);
   end Get_URI;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      if not Util.Processes.Is_Running (Server) then
         declare
            Args : Util.Strings.Vectors.Vector;
         begin
            Log.Info ("Starting server {}", Path);
            Args.Append (Path);
            --  Args.Append ("-v");
            Args.Append ("-p");
            Args.Append (Util.Strings.Image (Port));
            Util.Processes.Set_Shell (Server, "");
            Util.Processes.Spawn (Server, Args);
            delay 0.5;
         end;
      end if;
   end Set_Up;

   overriding
   procedure Tear_Down (T : in out Test) is
   begin
      if Util.Processes.Is_Running (Server) then
         delay 0.5;
         Log.Info ("Stopping server {0} running in {1}",
                   Path, Util.Processes.Get_Pid (Server)'Image);
         Util.Processes.Stop (Server);
         Util.Processes.Wait (Server);
      end if;
   end Tear_Down;

end Servlet.Samples_Servers;
