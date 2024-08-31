-----------------------------------------------------------------------
--  servlet-core-configs -- Read servlet configuration files
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Core.Mappers;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers;
with EL.Contexts.Default;
package body Servlet.Core.Configs is

   --  ------------------------------
   --  Read the configuration file associated with the servlet container.
   --  ------------------------------
   procedure Read_Configuration (App  : in out Servlet_Registry'Class;
                                 File : in String) is
      Reader  : Util.Serialize.IO.XML.Parser;
      Mapper  : Util.Serialize.Mappers.Processing;
      Context : aliased EL.Contexts.Default.Default_Context;

      --  Setup the <b>Reader</b> to parse and build the configuration for managed beans,
      --  navigation rules, servlet rules.  Each package instantiation creates a local variable
      --  used while parsing the XML file.
      package Config is
        new Mappers.Reader_Config (Mapper, App'Unchecked_Access, Context'Unchecked_Access);

      pragma Warnings (Off, Config);
   begin
      --  Read the configuration file and record managed beans, navigation rules.
      Reader.Parse (File, Mapper);
   end Read_Configuration;

end Servlet.Core.Configs;
