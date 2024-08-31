-----------------------------------------------------------------------
--  servlet-core-configs -- Read servlet configuration files
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Servlet.Core.Configs is

   --  Read the configuration file associated with the servlet container.
   procedure Read_Configuration (App  : in out Servlet_Registry'Class;
                                 File : in String);

end Servlet.Core.Configs;
