-----------------------------------------------------------------------
--  servlet-server -- Servlet Server for AWS
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2019, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
