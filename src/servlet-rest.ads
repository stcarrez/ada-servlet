-----------------------------------------------------------------------
--  servlet-rest -- REST Support
--  Copyright (C) 2016, 2020, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Strings;
with Util.Http.Mimes;
with Util.Serialize.IO;
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Core;
with EL.Contexts;
with Security.Permissions;
with Servlet.Streams.Dynamic;

--  == REST ==
--  The <tt>Servlet.Rest</tt> package provides support to implement easily some RESTful API.
package Servlet.Rest is

   subtype Request is Servlet.Requests.Request;

   subtype Response is Servlet.Responses.Response;

   subtype Output_Stream is Util.Serialize.IO.Output_Stream;

   subtype Mime_Access is Util.Http.Mimes.Mime_Access;
   use all type Util.Strings.Name_Access;

   subtype Mime_List is Util.Http.Mimes.Mime_List;
   subtype Mime_List_Access is Util.Http.Mimes.Mime_List_Access;
   use all type Util.Http.Mimes.Mime_List_Access;

   --  The HTTP rest method.
   type Method_Type is (GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, OPTIONS, PATCH);

   subtype Stream_Type is Servlet.Streams.Dynamic.Stream_Type;
   use type Servlet.Streams.Dynamic.Stream_Type;

   type Stream_Modes is array (Stream_Type) of Boolean with Pack;

   type Descriptor is abstract tagged limited private;
   type Descriptor_Access is access all Descriptor'Class;

   --  Get the permission index associated with the REST operation.
   function Get_Permission (Handler : in Descriptor)
                            return Security.Permissions.Permission_Index;

   --  Get the mime type selected for the operation.
   function Get_Mime_Type (Handler : in Descriptor;
                           Req     : in Servlet.Rest.Request'Class) return Mime_Access;

   --  Get the stream type that should be configured before calling the operation handler.
   --  The choice is made on the operation capabilities defined by the Stream_Modes description.
   --  If there are multiple choices, the DYNAMIC stream type is prefered if it is enabled.
   --  Otherwise we look at the Accept header and decide according to the generated mime types.
   function Get_Stream_Type (Handler : in Descriptor;
                             Req     : in Servlet.Rest.Request'Class) return Stream_Type;

   --  Dispatch the request to the API handler.
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out Servlet.Rest.Request'Class;
                       Reply   : in out Servlet.Rest.Response'Class;
                       Stream  : in out Output_Stream'Class) is abstract;

   type Operation_Access is
      access procedure (Req    : in out Servlet.Rest.Request'Class;
                        Reply  : in out Servlet.Rest.Response'Class;
                        Stream : in out Servlet.Rest.Output_Stream'Class);

   --  Register the API definition in the servlet registry.
   procedure Register (Registry   : in out Servlet.Core.Servlet_Registry'Class;
                       Definition : in Descriptor_Access);

   procedure Choose_Content_Type (Req    : in out Servlet.Rest.Request'Class;
                                  Reply  : in out Servlet.Rest.Response'Class;
                                  Stream : in out Servlet.Rest.Output_Stream'Class);

   --  Set the response Content-Type header and configure the stream accordingly.
   procedure Set_Content_Type (Reply  : in out Servlet.Rest.Response'Class;
                               Mime   : in String;
                               Stream : in out Servlet.Rest.Output_Stream'Class);

private

   type Descriptor is abstract tagged limited record
      Next       : Descriptor_Access;
      Method     : Method_Type;
      Mimes      : Mime_List_Access;
      Pattern    : Util.Strings.Name_Access;
      Permission : Security.Permissions.Permission_Index := 0;
      Streams    : Stream_Modes := (others => True);
   end record;

   --  Register the API descriptor in a list.
   procedure Register (List : in out Descriptor_Access;
                       Item : in Descriptor_Access);

   --  Register the list of API descriptors for a given servlet and a root path.
   procedure Register (Registry  : in out Servlet.Core.Servlet_Registry;
                       Name      : in String;
                       URI       : in String;
                       ELContext : in EL.Contexts.ELContext'Class;
                       List      : in Descriptor_Access);

   type Static_Descriptor is new Descriptor with record
      Handler : Operation_Access;
   end record;

   --  Dispatch the request to the API handler.
   overriding
   procedure Dispatch (Handler : in Static_Descriptor;
                       Req     : in out Servlet.Rest.Request'Class;
                       Reply   : in out Servlet.Rest.Response'Class;
                       Stream  : in out Output_Stream'Class);

end Servlet.Rest;
