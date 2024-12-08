-----------------------------------------------------------------------
--  server -- Example of server setup
--  Copyright (C) 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Server;

package Server is

   CONFIG_PATH  : constant String := "samples.properties";

   procedure Configure (WS     : in out Servlet.Server.Container'Class;
                        Config : in out Servlet.Server.Configuration);

end Server;
