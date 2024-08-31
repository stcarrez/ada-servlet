-----------------------------------------------------------------------
--  servlet-parts-mockup -- Mockup servlet parts (ie, local files)
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

private with Ada.Strings.Unbounded;
package Servlet.Parts.Mockup is

   type Part is new Servlet.Parts.Part with private;
   type Part_Array is array (Positive range <>) of Part;

   --  Get the size of the mime part.
   overriding
   function Get_Size (Data : in Part) return Natural;

   --  Get the content name submitted in the mime part.
   overriding
   function Get_Name (Data : in Part) return String;

   --  Get the path of the local file which contains the part.
   overriding
   function Get_Local_Filename (Data : in Part) return String;

   --  Get the content type of the part.
   overriding
   function Get_Content_Type (Data : in Part) return String;

   --  Create the part content by using a local file path.
   procedure Create (Into         : out Part;
                     Name         : in String;
                     Path         : in String;
                     Content_Type : in String);

private

   use Ada.Strings.Unbounded;

   type Part is new Servlet.Parts.Part with record
      Name         : Unbounded_String;
      Path         : Unbounded_String;
      Content_Type : Unbounded_String;
   end record;

end Servlet.Parts.Mockup;
