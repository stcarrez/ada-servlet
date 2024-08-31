-----------------------------------------------------------------------
--  servlet-rest-definition -- REST API Definition
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
generic
   type Object_Type is limited private;
   URI : String;
package Servlet.Rest.Definition is

   type Descriptor is new Servlet.Rest.Descriptor with record
      Handler  : access procedure (Object : in out Object_Type;
                                   Req    : in out Servlet.Rest.Request'Class;
                                   Reply  : in out Servlet.Rest.Response'Class;
                                   Stream : in out Servlet.Rest.Output_Stream'Class);
   end record;

   overriding
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out Servlet.Rest.Request'Class;
                       Reply   : in out Servlet.Rest.Response'Class;
                       Stream  : in out Servlet.Rest.Output_Stream'Class);

   --  Definition of an API operation mapped to a given URI pattern and associated with
   --  the operation handler.
   generic
      Handler    : access procedure (Object : in out Object_Type;
                                     Req    : in out Servlet.Rest.Request'Class;
                                     Reply  : in out Servlet.Rest.Response'Class;
                                     Stream : in out Servlet.Rest.Output_Stream'Class);
      Method     : Method_Type := Servlet.Rest.GET;
      Pattern    : String;
      Permission : Security.Permissions.Permission_Index := Security.Permissions.NONE;
   package Definition is
      Instance : aliased Descriptor;
   end Definition;

   --  Register the list of APIs that have been created by instantiating the <tt>Definition</tt>
   --  package.  The REST servlet identified by <tt>Name</tt> is searched in the servlet registry
   --  and used as the servlet for processing the API requests.
   procedure Register (Registry  : in out Servlet.Core.Servlet_Registry;
                       Name      : in String;
                       ELContext : in EL.Contexts.ELContext'Class);

private

   Entries : Servlet.Rest.Descriptor_Access;

end Servlet.Rest.Definition;
