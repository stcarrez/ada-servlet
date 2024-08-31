-----------------------------------------------------------------------
--  servlet-servlets -- Servlet.Core
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Sessions;
with Servlet.Sessions.Factory;
with Servlet.Routes;
limited with Servlet.Filters;

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Calendar;
with Ada.Exceptions;

with Util.Log;
with Util.Properties;
with Util.Strings.Vectors;

with EL.Contexts;

private with Ada.Containers.Indefinite_Hashed_Maps;

--  The <b>Servlet.Core</b> package implements a subset of the
--  Java Servlet Specification adapted for the Ada language.
--
--  The rationale for this implementation is to provide a set of
--  interfaces and ways of developing a Web application which
--  benefit from the architecture expertise defined in Java applications.
--
--  The <b>Servlet.Core</b>, <b>Servlet.Requests</b>, <b>Servlet.Responses</b>
--  and <b>Servlet.Sessions</b> packages are independent of the web server
--  which will be used (such as <b>AWS</b>, <b>Apache</b> or <b>Lighthttpd</b>).
--
package Servlet.Core is

   Servlet_Error : exception;

   type Status_Type is (Ready, Disabled, Started, Suspended, Stopped);

   --  Filter chain as defined by JSR 315 6. Filtering
   type Filter_Chain is limited private;
   type Filter_Config is private;

   type Filter_Access is access all Servlet.Filters.Filter'Class;
   type Filter_List_Access is access all Servlet.Filters.Filter_List;

   --  Causes the next filter in the chain to be invoked, or if the calling
   --  filter is the last filter in the chain, causes the resource at the end
   --  of the chain to be invoked.
   procedure Do_Filter (Chain    : in out Filter_Chain;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class);

   --  Get the filter name.
   function Get_Filter_Name (Config : in Filter_Config) return String;

   --  Returns a String containing the value of the named context-wide initialization
   --  parameter, or the default value if the parameter does not exist.
   --
   --  The filter parameter name is automatically prefixed by the filter name followed by '.'.
   function Get_Init_Parameter (Config  : in Filter_Config;
                                Name    : in String;
                                Default : in String := "") return String;
   function Get_Init_Parameter (Config : in Filter_Config;
                                Name    : in String;
                                Default : in String := "")
                                return Ada.Strings.Unbounded.Unbounded_String;

   --  type Servlet_Registry;
   type Servlet_Registry is new Servlet.Sessions.Factory.Session_Factory with private;

   type Servlet_Registry_Access is access all Servlet_Registry'Class;

   --  Get the servlet context associated with the filter chain.
   function Get_Servlet_Context (Chain : in Filter_Chain) return Servlet_Registry_Access;

   --  Get the servlet context associated with the filter config.
   function Get_Servlet_Context (Config : in Filter_Config) return Servlet_Registry_Access;

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   --
   --  JSR 315 - 2. The Servlet Interface
   type Servlet is tagged limited private;
   type Servlet_Access is access all Servlet'Class;

   --  Get the servlet name.
   function Get_Name (Server : in Servlet) return String;

   --  Get the servlet context associated with this servlet.
   function Get_Servlet_Context (Server : in Servlet) return Servlet_Registry_Access;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  not overriding
   procedure Initialize (Server  : in out Servlet;
                         Context : in Servlet_Registry'Class);

   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   procedure Service (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Returns the time the Request object was last modified, in milliseconds since
   --  midnight January 1, 1970 GMT.  If the time is unknown, this method returns
   --  a negative number (the default).
   --
   --  Servlets that support HTTP GET requests and can quickly determine their
   --  last modification time should override this method. This makes browser and
   --  proxy caches work more effectively, reducing the load on server and network
   --  resources.
   function Get_Last_Modified (Server  : in Servlet;
                               Request : in Requests.Request'Class)
                               return Ada.Calendar.Time;

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   --
   --  Overriding this method to support a GET request also automatically supports
   --  an HTTP HEAD request. A HEAD request is a GET request that returns no body
   --  in the response, only the request header fields.
   --
   --  When overriding this method, read the request data, write the response headers,
   --  get the response's writer or output stream object, and finally, write the
   --  response data. It's best to include content type and encoding.
   --  When using a PrintWriter object to return the response, set the content type
   --  before accessing the PrintWriter object.
   --
   --  The servlet container must write the headers before committing the response,
   --  because in HTTP the headers must be sent before the response body.
   --
   --  Where possible, set the Content-Length header (with the
   --  Response.Set_Content_Length method), to allow the servlet container
   --  to use a persistent connection to return its response to the client,
   --  improving performance. The content length is automatically set if the entire
   --  response fits inside the response buffer.
   --
   --  When using HTTP 1.1 chunked encoding (which means that the response has a
   --  Transfer-Encoding header), do not set the Content-Length header.
   --
   --  The GET method should be safe, that is, without any side effects for which
   --  users are held responsible. For example, most form queries have no side effects.
   --  If a client request is intended to change stored data, the request should use
   --  some other HTTP method.
   --
   --  The GET method should also be idempotent, meaning that it can be safely repeated.
   --  Sometimes making a method safe also makes it idempotent. For example, repeating
   --  queries is both safe and idempotent, but buying a product online or modifying
   --  data is neither safe nor idempotent.
   --
   --  If the request is incorrectly formatted, Do_Get  returns an HTTP "Bad Request"
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Receives an HTTP HEAD request from the protected service method and handles
   --  the request. The client sends a HEAD request when it wants to see only the
   --  headers of a response, such as Content-Type or Content-Length. The HTTP HEAD
   --  method counts the output bytes in the response to set the Content-Length header
   --  accurately.
   --
   --  If you override this method, you can avoid computing the response body and just
   --  set the response headers directly to improve performance. Make sure that the
   --  Do_Head method you write is both safe and idempotent (that is, protects itself
   --  from being called multiple times for one HTTP HEAD request).
   --
   --  If the HTTP HEAD request is incorrectly formatted, doHead returns an HTTP
   --  "Bad Request" message.
   procedure Do_Head (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a POST request. The HTTP POST method allows the client to send data of unlimited
   --  length to the Web server a single time and is useful when posting information
   --  such as credit card numbers.
   --
   --  When overriding this method, read the request data, write the response headers,
   --  get the response's writer or output stream object, and finally, write the
   --  response data. It's best to include content type and encoding. When using
   --  a PrintWriter object to return the response, set the content type before
   --  accessing the PrintWriter object.
   --
   --  The servlet container must write the headers before committing the response,
   --  because in HTTP the headers must be sent before the response body.
   --
   --  Where possible, set the Content-Length header (with the
   --  Response.Set_Content_Length method), to allow the servlet container to use
   --  a persistent connection to return its response to the client, improving
   --  performance. The content length is automatically set if the entire response
   --  fits inside the response buffer.
   --
   --  When using HTTP 1.1 chunked encoding (which means that the response has a
   --  Transfer-Encoding header), do not set the Content-Length header.
   --
   --  This method does not need to be either safe or idempotent. Operations
   --  requested through POST can have side effects for which the user can be held
   --  accountable, for example, updating stored data or buying items online.
   --
   --  If the HTTP POST request is incorrectly formatted, doPost returns
   --  an HTTP "Bad Request" message.
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a PUT request. The PUT operation allows a client to place a file on the server
   --  and is similar to sending a file by FTP.
   --
   --  When overriding this method, leave intact any content headers sent with
   --  the request (including Content-Length, Content-Type, Content-Transfer-Encoding,
   --  Content-Encoding, Content-Base, Content-Language, Content-Location,
   --  Content-MD5, and Content-Range). If your method cannot handle a content
   --  header, it must issue an error message (HTTP 501 - Not Implemented) and
   --  discard the request. For more information on HTTP 1.1, see RFC 2616 .
   --
   --  This method does not need to be either safe or idempotent. Operations that
   --  Do_Put performs can have side effects for which the user can be held accountable.
   --  When using this method, it may be useful to save a copy of the affected URL
   --  in temporary storage.
   --
   --  If the HTTP PUT request is incorrectly formatted, Do_Put returns
   --  an HTTP "Bad Request" message.
   procedure Do_Put (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a DELETE request. The DELETE operation allows a client to remove a document
   --  or Web page from the server.
   --
   --  This method does not need to be either safe or idempotent. Operations requested
   --  through DELETE can have side effects for which users can be held accountable.
   --  When using this method, it may be useful to save a copy of the affected URL in
   --  temporary storage.
   --
   --  If the HTTP DELETE request is incorrectly formatted, Do_Delete returns an HTTP
   --  "Bad Request" message.
   procedure Do_Delete (Server   : in Servlet;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle a
   --  OPTIONS request. The OPTIONS request determines which HTTP methods the server
   --  supports and returns an appropriate header. For example, if a servlet overrides
   --  Do_Get, this method returns the following header:
   --
   --  Allow: GET, HEAD, TRACE, OPTIONS
   --
   --  There's no need to override this method unless the servlet implements new
   --  HTTP methods, beyond those implemented by HTTP 1.1.
   procedure Do_Options (Server   : in Servlet;
                         Request  : in out Requests.Request'Class;
                         Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a TRACE request. A TRACE returns the headers sent with the TRACE request to
   --  the client, so that they can be used in debugging. There's no need to override
   --  this method.
   procedure Do_Trace (Server   : in Servlet;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a PATCH request (RFC 5789).
   procedure Do_Patch (Server   : in Servlet;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class);

   --  JSR 315 9. Dispatching Requests
   type Request_Dispatcher is limited private;

   --  Forwards a request from a servlet to another resource
   --  (servlet, or HTML file) on the server. This method allows one servlet to do
   --  preliminary processing of a request and another resource to generate the response.
   --
   --  For a Request_Dispatcher obtained via Get_Request_Dispatcher(),
   --  the ServletRequest  object has its path elements and parameters adjusted
   --  to match the path of the target resource.
   --
   --  forward should be called before the response has been committed to the
   --  client (before response body output has been flushed). If the response
   --  already has been committed, this method throws an IllegalStateException.
   --  Uncommitted output in the response buffer is automatically cleared before
   --  the forward.
   --
   --  The request and response parameters must be either the same objects as were
   --  passed to the calling servlet's service method or be subclasses of the
   --  RequestWrapper or ResponseWrapper  classes that wrap them.
   procedure Forward (Dispatcher : in Request_Dispatcher;
                      Request    : in out Requests.Request'Class;
                      Response   : in out Responses.Response'Class);

   --  Includes the content of a resource (servlet, or, HTML file) in the response.
   --  In essence, this method enables programmatic server-side includes.
   --
   --  The Response object has its path elements and parameters remain
   --  unchanged from the caller's. The included servlet cannot change the response
   --  status code or set headers; any attempt to make a change is ignored.
   --
   --  The request and response parameters must be either the same objects as were
   --  passed to the calling servlet's service method or be subclasses of the
   --  RequestWrapper or ResponseWrapper  classes that wrap them.
   procedure Include (Dispatcher : in Request_Dispatcher;
                      Request    : in out Requests.Request'Class;
                      Response   : in out Responses.Response'Class);

   --  Returns the servlet that will be called when forwarding the request.
   function Get_Servlet (Dispatcher : in Request_Dispatcher) return Servlet_Access;

   --  Returns a Request_Dispatcher object that acts as a wrapper for the resource
   --  located at the given path.  A Request_Dispatcher  object can be used to forward
   --  a request to the resource or to include the resource in a response.
   --  The resource can be dynamic or static.
   function Get_Request_Dispatcher (Context : in Servlet_Registry;
                                    Path    : in String)
                                    return Request_Dispatcher;

   --  Returns a Request_Dispatcher object that acts as a wrapper for the named servlet.
   --
   --  Servlets may be given names via server administration or via a web application
   --  deployment descriptor.  A servlet instance can determine its name using
   --  ServletConfig.getServletName().
   function Get_Name_Dispatcher (Context : in Servlet_Registry;
                                 Name    : in String)
                                 return Request_Dispatcher;

   --  Returns the context path of the web application.
   --  The context path is the portion of the request URI that is used to select the context
   --  of the request.  The context path always comes first in a request URI.  The path starts
   --  with a "/" character but does not end with a "/" character. For servlets in the default
   --  (root) context, this method returns "".
   function Get_Context_Path (Context : in Servlet_Registry) return String;

   --  Returns a String containing the value of the named context-wide initialization
   --  parameter, or null if the parameter does not exist.
   --
   --  This method can make available configuration information useful to an entire
   --  "web application". For example, it can provide a webmaster's email address
   --  or the name of a system that holds critical data.
   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String;
                                Default : in String := "") return String;
   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String;
                                Default : in String := "")
                                return Ada.Strings.Unbounded.Unbounded_String;

   --  Set the init parameter identified by <b>Name</b> to the value <b>Value</b>.
   procedure Set_Init_Parameter (Context : in out Servlet_Registry;
                                 Name    : in String;
                                 Value   : in String);

   --  Set the init parameters by copying the properties defined in <b>Params</b>.
   --  Existing parameters will be overriding by the new values.
   procedure Set_Init_Parameters (Context : in out Servlet_Registry;
                                  Params  : in Util.Properties.Manager'Class);

   --  Get access to the init parameters.
   procedure Get_Init_Parameters (Context : in Servlet_Registry;
                                  Process : not null access
                                    procedure (Params : in Util.Properties.Manager'Class));

   --  Returns the absolute path of the resource identified by the given relative path.
   --  The resource is searched in a list of directories configured by the application.
   --  The path must begin with a "/" and is interpreted as relative to the current
   --  context root.
   --
   --  This method allows the servlet container to make a resource available to
   --  servlets from any source.
   --
   --  This method returns an empty string if the resource could not be localized.
   function Get_Resource (Context : in Servlet_Registry;
                          Path    : in String) return String;

   --  Registers the given servlet instance with this ServletContext under
   --  the given servletName.
   --
   --  If this ServletContext already contains a preliminary
   --  ServletRegistration for a servlet with the given servletName,
   --  it will be completed (by assigning the class name of the given
   --  servlet instance to it) and returned.
   procedure Add_Servlet (Registry : in out Servlet_Registry;
                          Name     : in String;
                          Server   : in Servlet_Access);

   --  Registers the given filter instance with this Servlet context.
   procedure Add_Filter (Registry : in out Servlet_Registry;
                         Name     : in String;
                         Filter   : in Filter_Access);

   --  Add a filter mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   procedure Add_Filter_Mapping (Registry : in out Servlet_Registry;
                                 Pattern  : in String;
                                 Name     : in String);

   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Name     : in String);

   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Server   : in Servlet_Access);

   --  Add a route associated with the given path pattern.  The pattern is split into components.
   --  Some path components can be a fixed string (/home) and others can be variable.
   --  When a path component is variable, the value can be retrieved from the route context.
   --  Once the route path is created, the <tt>Process</tt> procedure is called with the route
   --  reference.
   procedure Add_Route (Registry  : in out Servlet_Registry;
                        Pattern   : in String;
                        ELContext : in EL.Contexts.ELContext'Class;
                        Process   : not null access
                          procedure (Route : in out Routes.Route_Type_Ref));

   --  Set the error page that will be used if a servlet returns an error.
   procedure Set_Error_Page (Server : in out Servlet_Registry;
                             Error  : in Integer;
                             Page   : in String);

   --  Send the error page content defined by the response status.
   procedure Send_Error_Page (Server   : in Servlet_Registry;
                              Request  : in out Requests.Request'Class;
                              Response : in out Responses.Response'Class);

   --  Report an error when an exception occurred while processing the request.
   procedure Error (Registry : in Servlet_Registry;
                    Request  : in out Requests.Request'Class;
                    Response : in out Responses.Response'Class;
                    Ex       : in Ada.Exceptions.Exception_Occurrence);

   --  Register the application represented by <b>Registry</b> under the base URI defined
   --  by <b>URI</b>.  This is called by the Web container when the application is registered.
   --  The default implementation keeps track of the base URI to implement the context path
   --  operation.
   procedure Register_Application (Registry : in out Servlet_Registry;
                                   URI      : in String);

   --  Start the application.
   procedure Start (Registry : in out Servlet_Registry);

   --  Get the application status.
   function Get_Status (Registry : in Servlet_Registry) return Status_Type;

   --  Disable the application.
   procedure Disable (Registry : in out Servlet_Registry);

   --  Enable the application.
   procedure Enable (Registry : in out Servlet_Registry);

   --  Stop the application.
   procedure Stop (Registry : in out Servlet_Registry);

   --  Finalize the servlet registry releasing the internal mappings.
   overriding
   procedure Finalize (Registry : in out Servlet_Registry);

   --  Dump the routes and filter configuration in the log with the given log level.
   procedure Dump_Routes (Registry : in out Servlet_Registry;
                          Level    : in Util.Log.Level_Type);

private

   use Ada.Strings.Unbounded;

   type Filter_Chain is limited record
      Filter_Pos : Natural;
      Filters    : Filter_List_Access;
      Servlet    : Servlet_Access;
   end record;

   type Request_Dispatcher is limited record
      Context : aliased Routes.Route_Context_Type;
      Filters : Filter_List_Access;
      Servlet : Servlet_Access := null;
      Pos     : Natural := 0;
   end record;

   type Servlet is new Ada.Finalization.Limited_Controlled with record
      Name      : Unbounded_String;
      Context   : Servlet_Registry_Access := null;
   end record;

   package Filter_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => String,
                                            Element_Type => Filter_Access,
                                            Hash         => Ada.Strings.Hash,
                                            Equivalent_Keys => "=");

   package Filter_List_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => String,
                                            Element_Type => Filter_List_Access,
                                            Hash         => Ada.Strings.Hash,
                                            Equivalent_Keys => "=");

   package Servlet_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => String,
                                            Element_Type => Servlet_Access,
                                            Hash         => Ada.Strings.Hash,
                                            Equivalent_Keys => "=");

   function Hash (N : Integer) return Ada.Containers.Hash_Type;

   package Error_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type            => Integer,
                                            Element_Type        => String,
                                            Hash                => Hash,
                                            Equivalent_Keys     => "=");

   use Routes;

   type Servlet_Registry is new Sessions.Factory.Session_Factory with record
      Config            : Util.Properties.Manager;
      Servlets          : Servlet_Maps.Map;
      Filters           : Filter_Maps.Map;
      Filter_Rules      : Filter_List_Maps.Map;
      Filter_Patterns   : Util.Strings.Vectors.Vector;
      Error_Pages       : Error_Maps.Map;
      Context_Path      : Unbounded_String;
      Routes            : Router_Type;
      Status            : Status_Type := Ready;
   end record;

   --  Install the servlet filters after all the mappings have been registered.
   procedure Install_Filters (Registry : in out Servlet_Registry);

   type Filter_Config is record
      Name      : Unbounded_String;
      Context   : Servlet_Registry_Access := null;
   end record;

end Servlet.Core;
