-----------------------------------------------------------------------
--  servlet-core-configs -- Read servlet configuration files
--  Copyright (C) 2018 Stephane Carrez
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
