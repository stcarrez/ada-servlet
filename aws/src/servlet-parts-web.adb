-----------------------------------------------------------------------
--  servlet-parts-web -- Servlet Parts on top of AWS attachments
--  Copyright (C) 2011, 2012, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWS.Attachments.Extend;

package body Servlet.Parts.Web is

   --  ------------------------------
   --  Get the size of the mime part.
   --  ------------------------------
   overriding
   function Get_Size (Data : in AWS_Part) return Natural is
   begin
      return AWS.Attachments.Extend.Get_Length (Data.Element);
   end Get_Size;

   --  ------------------------------
   --  Get the content name submitted in the mime part.
   --  ------------------------------
   overriding
   function Get_Name (Data : in AWS_Part) return String is
   begin
      return AWS.Attachments.Filename (Data.Element);
   end Get_Name;

   --  ------------------------------
   --  Get the path of the local file which contains the part.
   --  ------------------------------
   overriding
   function Get_Local_Filename (Data : in AWS_Part) return String is
   begin
      return AWS.Attachments.Local_Filename (Data.Element);
   end Get_Local_Filename;

   --  ------------------------------
   --  Get the content type of the part.
   --  ------------------------------
   overriding
   function Get_Content_Type (Data : in AWS_Part) return String is
   begin
      return AWS.Attachments.Content_Type (Data.Element);
   end Get_Content_Type;

   --  ------------------------------
   --  Build a part instance from the AWS attachment and execute the <b>Process</b> operation.
   --  ------------------------------
   procedure Process_Part (Part    : in AWS.Attachments.Element;
                           Process : not null access procedure (Part : in Parts.Part'Class)) is
      P : AWS_Part;
   begin
      P.Element := Part;
      Process (P);
   end Process_Part;

end Servlet.Parts.Web;
