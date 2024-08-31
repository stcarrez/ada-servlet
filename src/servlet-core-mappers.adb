-----------------------------------------------------------------------
--  servlet-servlets-mappers -- Read servlet configuration files
--  Copyright (C) 2011, 2012, 2013, 2015, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Containers;
with EL.Utils;

package body Servlet.Core.Mappers is

   --  ------------------------------
   --  Save in the servlet config object the value associated with the given field.
   --  When the <b>FILTER_MAPPING</b>, <b>SERVLET_MAPPING</b> or <b>CONTEXT_PARAM</b> field
   --  is reached, insert the new configuration rule in the servlet registry.
   --  ------------------------------
   procedure Set_Member (N     : in out Servlet_Config;
                         Field : in Servlet_Fields;
                         Value : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
      use type Ada.Containers.Count_Type;

      procedure Add_Filter (Pattern : in Util.Beans.Objects.Object);
      procedure Add_Mapping (Pattern : in Util.Beans.Objects.Object);
      procedure Add_Mapping (Handler : access procedure (Pattern : in Util.Beans.Objects.Object);
                             Message : in String);

      procedure Add_Filter (Pattern : in Util.Beans.Objects.Object) is
      begin
         N.Handler.Add_Filter_Mapping (Pattern => To_String (Pattern),
                                       Name    => To_String (N.Filter_Name));
      end Add_Filter;

      procedure Add_Mapping (Pattern : in Util.Beans.Objects.Object) is
      begin
         N.Handler.Add_Mapping (Pattern => To_String (Pattern),
                                Name    => To_String (N.Servlet_Name));
      end Add_Mapping;

      procedure Add_Mapping (Handler : access procedure (Pattern : in Util.Beans.Objects.Object);
                             Message : in String) is
         Last : constant Ada.Containers.Count_Type := N.URL_Patterns.Length;
      begin
         if Last = 0 then
            raise Util.Serialize.Mappers.Field_Error with Message;
         end if;
         for I in 1 .. Last loop
            N.URL_Patterns.Query_Element (Positive (I), Handler);
         end loop;
         N.URL_Patterns.Clear;
      end Add_Mapping;

   begin
      --  <context-param>
      --    <param-name>property</param-name>
      --    <param-value>false</param-value>
      --  </context-param>
      --  <filter-mapping>
      --    <filter-name>Dump Filter</filter-name>
      --    <servlet-name>Faces Servlet</servlet-name>
      --  </filter-mapping>
      case Field is
         when FILTER_NAME =>
            N.Filter_Name := Value;

         when SERVLET_NAME =>
            N.Servlet_Name := Value;

         when URL_PATTERN =>
            N.URL_Patterns.Append (Value);

         when PARAM_NAME =>
            N.Param_Name := Value;

         when PARAM_VALUE =>
            N.Param_Value := EL.Utils.Eval (To_String (Value), N.Context.all);

         when MIME_TYPE =>
            N.Mime_Type := Value;

         when EXTENSION =>
            N.Extension := Value;

         when ERROR_CODE =>
            N.Error_Code := Value;

         when LOCATION =>
            N.Location := Value;

         when FILTER_MAPPING =>
            Add_Mapping (Add_Filter'Access, "Missing url-pattern for the filter mapping");

         when SERVLET_MAPPING =>
            Add_Mapping (Add_Mapping'Access, "Missing url-pattern for the servlet mapping");

         when CONTEXT_PARAM =>
            declare
               Name : constant String := To_String (N.Param_Name);
            begin
               --  If the context parameter already has a value, do not set it again.
               --  The value comes from an application setting and we want to keep it.
               if N.Override_Context
                 or else String '(N.Handler.all.Get_Init_Parameter (Name)) = ""
               then
                  if Util.Beans.Objects.Is_Null (N.Param_Value) then
                     N.Handler.Set_Init_Parameter (Name  => Name,
                                                   Value => "");
                  else
                     N.Handler.Set_Init_Parameter (Name  => Name,
                                                   Value => To_String (N.Param_Value));
                  end if;
               end if;
            end;

         when MIME_MAPPING =>
            null;

         when ERROR_PAGE =>
            N.Handler.Set_Error_Page (Error => To_Integer (N.Error_Code),
                                      Page  => To_String (N.Location));

      end case;
   end Set_Member;

   SMapper : aliased Servlet_Mapper.Mapper;

   --  ------------------------------
   --  Setup the XML parser to read the servlet and mapping rules <b>context-param</b>,
   --  <b>filter-mapping</b> and <b>servlet-mapping</b>.
   --  ------------------------------
   package body Reader_Config is
   begin
      Mapper.Add_Mapping ("faces-config", SMapper'Access);
      Mapper.Add_Mapping ("module", SMapper'Access);
      Mapper.Add_Mapping ("web-app", SMapper'Access);
      Config.Handler := Handler;
      Config.Context := Context;
      Servlet_Mapper.Set_Context (Mapper, Config'Unchecked_Access);
   end Reader_Config;

begin
   SMapper.Add_Mapping ("filter-mapping", FILTER_MAPPING);
   SMapper.Add_Mapping ("filter-mapping/filter-name", FILTER_NAME);
   SMapper.Add_Mapping ("filter-mapping/servlet-name", SERVLET_NAME);
   SMapper.Add_Mapping ("filter-mapping/url-pattern", URL_PATTERN);

   SMapper.Add_Mapping ("servlet-mapping", SERVLET_MAPPING);
   SMapper.Add_Mapping ("servlet-mapping/servlet-name", SERVLET_NAME);
   SMapper.Add_Mapping ("servlet-mapping/url-pattern", URL_PATTERN);

   SMapper.Add_Mapping ("context-param", CONTEXT_PARAM);
   SMapper.Add_Mapping ("context-param/param-name", PARAM_NAME);
   SMapper.Add_Mapping ("context-param/param-value", PARAM_VALUE);

   SMapper.Add_Mapping ("error-page", ERROR_PAGE);
   SMapper.Add_Mapping ("error-page/error-code", ERROR_CODE);
   SMapper.Add_Mapping ("error-page/location", LOCATION);

end Servlet.Core.Mappers;
