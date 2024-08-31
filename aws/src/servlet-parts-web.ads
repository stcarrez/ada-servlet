-----------------------------------------------------------------------
--  servlet-parts-web -- Servlet Parts on top of AWS attachments
--  Copyright (C) 2011, 2012, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWS.Attachments;

--  The <b>Servlet.Parts.Web</b> package implements Servlet parts on top of AWS attachments.
package Servlet.Parts.Web is

   --  ------------------------------
   --  Multi part content
   --  ------------------------------
   --  The <b>Part</b> type describes a mime part received in a request.
   --  The content is stored in a file and several operations are provided
   --  to manage the content.
   type AWS_Part is new Part with private;

   --  Get the size of the mime part.
   overriding
   function Get_Size (Data : in AWS_Part) return Natural;

   --  Get the content name submitted in the mime part.
   overriding
   function Get_Name (Data : in AWS_Part) return String;

   --  Get the path of the local file which contains the part.
   overriding
   function Get_Local_Filename (Data : in AWS_Part) return String;

   --  Get the content type of the part.
   overriding
   function Get_Content_Type (Data : in AWS_Part) return String;

   --  Build a part instance from the AWS attachment and execute the <b>Process</b> operation.
   procedure Process_Part (Part    : in AWS.Attachments.Element;
                           Process : not null access procedure (Part : in Parts.Part'Class));

private

   type AWS_Part is new Part with record
      Element      : AWS.Attachments.Element;
   end record;

end Servlet.Parts.Web;
