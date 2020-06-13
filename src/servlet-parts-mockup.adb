-----------------------------------------------------------------------
--  servlet-parts-mockup -- Mockup servlet parts (ie, local files)
--  Copyright (C) 2020 Stephane Carrez
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

with Ada.Directories;
package body Servlet.Parts.Mockup is

   --  ------------------------------
   --  Get the size of the mime part.
   --  ------------------------------
   overriding
   function Get_Size (Data : in Part) return Natural is
   begin
      return Natural (Ada.Directories.Size (To_String (Data.Path)));
   end Get_Size;

   --  ------------------------------
   --  Get the content name submitted in the mime part.
   --  ------------------------------
   overriding
   function Get_Name (Data : in Part) return String is
   begin
      return To_String (Data.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the path of the local file which contains the part.
   --  ------------------------------
   overriding
   function Get_Local_Filename (Data : in Part) return String is
   begin
      return To_String (Data.Path);
   end Get_Local_Filename;

   --  ------------------------------
   --  Get the content type of the part.
   --  ------------------------------
   overriding
   function Get_Content_Type (Data : in Part) return String is
   begin
      return To_String (Data.Content_Type);
   end Get_Content_Type;

   --  ------------------------------
   --  Create the part content by using a local file path.
   --  ------------------------------
   procedure Create (Into         : out Part;
                     Name         : in String;
                     Path         : in String;
                     Content_Type : in String) is
   begin
      Into.Name := To_Unbounded_String (Name);
      Into.Path := To_Unbounded_String (Path);
      Into.Content_Type := To_Unbounded_String (Content_Type);
   end Create;

end Servlet.Parts.Mockup;
