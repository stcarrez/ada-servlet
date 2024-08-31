-----------------------------------------------------------------------
--  servlet-server -- Servlet Server for AWS
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2019, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWS.Config;
private with AWS.Server;
package Servlet.Server.Web is

   type AWS_Container is new Container with private;

   --  Start the applications that have been registered.
   overriding
   procedure Start (Server : in out AWS_Container);

   --  Configure the AWS server.
   procedure Configure (Server : in out AWS_Container;
                        Process : not null access procedure (Config : in out AWS.Config.Object));

   overriding
   procedure Configure (Server : in out AWS_Container;
                        Config : in Configuration);

private

   type AWS_Container_Access is access all AWS_Container'Class;

   overriding
   procedure Initialize (Instance : in out AWS_Container);

   type AWS_Container is new Container with record
      WS   : AWS.Server.HTTP;
      Conf : AWS.Config.Object;
   end record;

end Servlet.Server.Web;
