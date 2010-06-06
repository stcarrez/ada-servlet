-----------------------------------------------------------------------
--  asf-views-facelets -- Facelets representation and management
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Ada.Strings.Fixed;
with ASF.Views.Nodes.Reader;
with Input_Sources.File;
with Sax.Readers;
with EL.Contexts.Default;
with Util.Files;
with Util.Log.Loggers;
package body ASF.Views.Facelets is

   use ASF.Views.Nodes;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Modules");

   --  Find in the factory for the facelet with the given name.
   procedure Find (Factory : in out Facelet_Factory;
                   Name    : in Unbounded_String;
                   Result  : out Facelet);

   --  Load the facelet node tree by reading the facelet XHTML file.
   procedure Load (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Result  : out Facelet);

   --  Update the factory to store the facelet node tree
   procedure Update (Factory : in out Facelet_Factory;
                     Name    : in Unbounded_String;
                     Item    : in Facelet);

   --  ------------------------------
   --  Get the facelet identified by the given name.  If the facelet is already
   --  loaded, the cached value is returned.  The facelet file is searched in
   --  a set of directories configured in the facelet factory.
   --  ------------------------------
   procedure Find_Facelet (Factory : in out Facelet_Factory;
                           Name    : in String;
                           Result  : out Facelet) is
      Res   : Facelet;
      Fname : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      Find (Factory, Fname, Res);
      if Res.Root = null then
         Load (Factory, Name, Res);
         if Res.Root = null then
            Result.Root := null;
            return;
         end if;
         Update (Factory, Fname, Res);
      end if;
      Result.Root := Res.Root;
   end Find_Facelet;

   --  ------------------------------
   --  Create the component tree from the facelet view.
   --  ------------------------------
   procedure Build_View (View    : in Facelet;
                         Context : in out ASF.Contexts.Facelets.Facelet_Context'Class;
                         Root    : in ASF.Components.UIComponent_Access) is
   begin
      if View.Root /= null then
         View.Root.Build_Children (Parent  => Root,
                                   Context => Context);
      end if;
   end Build_View;

   --  ------------------------------
   --  Set the search directories for facelet files.
   --  ------------------------------
   procedure Set_Search_Directory (Factory : in out Facelet_Factory;
                                   Paths   : in String) is
   begin
      Log.Info ("Set facelet search directory to: '{0}'", Paths);

      Factory.Paths := To_Unbounded_String (Paths);
   end Set_Search_Directory;

   --  ------------------------------
   --  Find the facelet file in one of the facelet directories.
   --  Returns the path to be used for reading the facelet file.
   --  ------------------------------
   function Find_Facelet_Path (Factory : Facelet_Factory;
                               Name    : String) return String is
      use Util.Strings;

      Pos : constant Natural := Ada.Strings.Fixed.Index (Name, "/", Name'First + 1);
   begin
      if Pos > 0 then
         --  Get the module
         declare
            Module   : constant String := Name (Name'First + 1 .. Pos - 1);
            Path_Pos : constant String_Map.Cursor
              := Factory.Path_Map.Find (Module'Unrestricted_Access);
         begin
            if String_Map.Has_Element (Path_Pos) then
               Log.Info ("Looking module {0} in {1}", Module,
                        String_Map.Element (Path_Pos).all);
               return Util.Files.Find_File_Path (Name (Pos + 1 .. Name'Last),
                                                 String_Map.Element (Path_Pos).all);
            end if;
         end;
      end if;
      return Util.Files.Find_File_Path (Name, To_String (Factory.Paths));
   end Find_Facelet_Path;

   --  ------------------------------
   --  Register a module and directory where the module files are stored.
   --  ------------------------------
   procedure Register_Module (Factory : in out Facelet_Factory;
                              Name    : in String;
                              Paths   : in String) is
      N : constant Util.Strings.Name_Access := new String '(Name);
      P : constant Util.Strings.Name_Access := new String '(Paths);
   begin
      Log.Info ("Search path for '{0}' is '{1}'", Name, Paths);

      Factory.Path_Map.Include (N, P);
   end Register_Module;

   --  ------------------------------
   --  Find in the factory for the facelet with the given name.
   --  ------------------------------
   procedure Find (Factory : in out Facelet_Factory;
                   Name    : in Unbounded_String;
                   Result  : out Facelet) is
   begin
      Result.Root := null;
      Factory.Lock.Read;
      declare
         Pos : constant Facelet_Maps.Cursor := Factory.Map.Find (Name);
      begin
         if Facelet_Maps.Has_Element (Pos) then
            Result := Element (Pos);
         end if;
      end;
      Factory.Lock.Release_Read;
   end Find;

   --  ------------------------------
   --  Register the component factory bindings in the facelet factory.
   --  ------------------------------
   procedure Register (Factory  : in out Facelet_Factory;
                       Bindings : in ASF.Factory.Factory_Bindings_Access) is
      use ASF;
   begin
      ASF.Factory.Register (Factory  => Factory.Factory,
                            Bindings => Bindings);
   end Register;

   --  ------------------------------
   --  Load the facelet node tree by reading the facelet XHTML file.
   --  ------------------------------
   procedure Load (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Result  : out Facelet) is
      use ASF.Views.Nodes.Reader;
      use Input_Sources.File;
      use Sax.Readers;

      Path   : constant String := Find_Facelet_Path (Factory, Name);
      Reader : Xhtml_Reader;
      Read   : File_Input;
      Context : aliased EL.Contexts.Default.Default_Context;
   begin
      Log.Info ("Loading facelet: '{0}'", Path);

      Open (Path, Read);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Namespace_Prefixes_Feature, False);
      Set_Feature (Reader, Validation_Feature, False);

      Parse (Reader, Read, Factory.Factory'Unchecked_Access, Context'Unchecked_Access);
      Close (Read);

      Result := Facelet '(Root => Get_Root (Reader));
   exception
      when others =>
         Close (Read);
         Result.Root := null;
         Log.Error ("Error while reading: '{0}'", Path);

   end Load;

   --  ------------------------------
   --  Update the factory to store the facelet node tree
   --  ------------------------------
   procedure Update (Factory : in out Facelet_Factory;
                     Name    : in Unbounded_String;
                     Item    : in Facelet) is
   begin
      Factory.Lock.Write;
      Factory.Map.Include (Name, Item);
      Factory.Lock.Release_Write;
   end Update;

   --  ------------------------------
   --  Clear the facelet cache
   --  ------------------------------
   procedure Clear_Cache (Factory : in out Facelet_Factory) is
   begin
      Factory.Lock.Write;
      loop
         declare
            Pos  : Facelet_Maps.Cursor := Factory.Map.First;
            Node : Facelet;
         begin
            exit when not Has_Element (Pos);
            Node := Element (Pos);
            Factory.Map.Delete (Pos);
            Node.Root.Delete;
         end;
      end loop;
      Factory.Lock.Release_Write;
   end Clear_Cache;

   protected body RW_Lock is
      entry Write when Reader_Count = 0 and Readable is
      begin
         Readable := False;
      end Write;

      procedure Release_Write is
      begin
         Readable := True;
      end Release_Write;

      entry Read when Readable is
      begin
         Reader_Count := Reader_Count + 1;
      end Read;

      procedure Release_Read is
      begin
         Reader_Count := Reader_Count - 1;
      end Release_Read;
   end RW_Lock;

end ASF.Views.Facelets;
