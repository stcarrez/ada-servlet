-----------------------------------------------------------------------
--  upload_servlet -- Servlet example to upload files on the server
--  Copyright (C) 2012, 2018, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Core;
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Streams;

package Upload_Servlet is

   use Servlet;

   type File_Type is (IMAGE, PDF, TAR_GZ, TAR, ZIP, UNKNOWN);

   --  Guess a file type depending on a content type or a file name.
   function Get_File_Type (Content_Type : in String;
                           Name         : in String) return File_Type;

   --  Execute a command and write the result to the output stream.
   procedure Execute (Command : in String;
                      Output  : in out Streams.Print_Stream);

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Servlet is new Core.Servlet with null record;

   --  Called by the servlet container when a GET request is received.
   --  Display the upload form page.
   overriding
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Called by the servlet container when a POST request is received.
   --  Receives the uploaded files and identify them using some external command.
   overriding
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

private

   --  Write the upload form page with an optional response message.
   procedure Write (Response : in out Responses.Response'Class;
                    Message  : in String);

end Upload_Servlet;
