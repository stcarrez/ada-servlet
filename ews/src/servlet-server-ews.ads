-----------------------------------------------------------------------
--  servlet-server -- Servlet Server for AWS
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2019, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Servlet.Server.EWS is

   type EWS_Container is new Container with private;

   --  Start the applications that have been registered.
   overriding
   procedure Start (Server : in out EWS_Container);

   overriding
   procedure Configure (Server : in out EWS_Container;
                        Config : in Configuration);

private

   type EWS_Container_Access is access all EWS_Container'Class;

   overriding
   procedure Initialize (Instance : in out EWS_Container);

   type EWS_Container is new Container with record
      Conf : Configuration;
   end record;

end Servlet.Server.EWS;
