-----------------------------------------------------------------------
--  server -- Example of server setup
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.IO_Exceptions;
with GNAT.Command_Line;
with Util.Log.Loggers;
with Util.Properties;
package body Server is

   use GNAT.Command_Line;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Server");

   procedure Load (Props : in out Util.Properties.Manager;
                   Path  : in String);

   procedure Load (Props : in out Util.Properties.Manager;
                   Path  : in String) is
   begin
      Props.Load_Properties (CONFIG_PATH);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot load configuration file {}", Path);
         Props.Set ("samples.rootCategory", "DEBUG,console");
         Props.Set ("samples.appender.console", "Console");
         Props.Set ("samples.appender.console.level", "INFO");
         Props.Set ("samples.appender.console.layout", "message");
         Props.Set ("samples.logger.Servlet", "WARN");
   end Load;

   procedure Configure (WS     : in out Servlet.Server.Container'Class;
                        Config : in out Servlet.Server.Configuration) is
      Props : Util.Properties.Manager;
   begin
      Log.Error ("Starting samples server (use -v to enable verbose mode"
                 & " and -p <port> to change listening port)");
      Load (Props, CONFIG_PATH);
      Util.Log.Loggers.Initialize (Props, "samples.");
      Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");
      loop
         case Getopt ("* v p: c:") is
            when 'v' =>
               Props.Set ("samples.appender.console.level", "DEBUG");
               Props.Set ("samples.logger.Servlet", "DEBUG");
               Props.Set ("samples.appender.console.layout", "full");

            when 'p' =>
               begin
                  Config.Listening_Port := Integer'Value (Parameter);
               exception
                  when others =>
                     Log.Error ("Invalid port: {}", Parameter);
               end;

            when 'c' =>
               Load (Props, Parameter);

            when others =>
               exit;
         end case;
      end loop;
      Util.Log.Loggers.Initialize (Props, "samples.");
      WS.Configure (Config);
   end Configure;

end Server;
