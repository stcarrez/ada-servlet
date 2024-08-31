-----------------------------------------------------------------------
--  servlet-rest -- REST Support
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Servlet.Rest.Definition is

   --  ------------------------------
   --  Register the list of APIs that have been created by instantiating the <tt>Definition</tt>
   --  package.  The REST servlet identified by <tt>Name</tt> is searched in the servlet registry
   --  and used as the servlet for processing the API requests.
   --  ------------------------------
   procedure Register (Registry  : in out Servlet.Core.Servlet_Registry;
                       Name      : in String;
                       ELContext : in EL.Contexts.ELContext'Class) is
   begin
      Servlet.Rest.Register (Registry => Registry,
                         Name     => Name,
                         URI      => URI,
                         ELContext => ELContext,
                         List     => Entries);
   end Register;

   overriding
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out Servlet.Rest.Request'Class;
                       Reply   : in out Servlet.Rest.Response'Class;
                       Stream  : in out Servlet.Rest.Output_Stream'Class) is
      Object : Object_Type;
   begin
      Handler.Handler (Object, Req, Reply, Stream);
   end Dispatch;

   package body Definition is
      P : aliased String := Pattern;
   begin
      Instance.Method     := Method;
      Instance.Permission := Permission;
      Instance.Handler    := Handler;
      Instance.Pattern    := P'Access;
      Servlet.Rest.Register (Entries, Instance'Access);
   end Definition;

end Servlet.Rest.Definition;
