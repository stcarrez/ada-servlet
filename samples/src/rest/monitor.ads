-----------------------------------------------------------------------
--  monitor - A simple monitor API
--  Copyright (C) 2016, 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;
with Servlet.Rest.Operation;
package Monitor is

   --  Get values of the monitor.
   procedure Get_Values (Req    : in out Servlet.Rest.Request'Class;
                         Reply  : in out Servlet.Rest.Response'Class;
                         Stream : in out Servlet.Rest.Output_Stream'Class);

   --  PUT /mon/:id
   procedure Put_Value (Req    : in out Servlet.Rest.Request'Class;
                        Reply  : in out Servlet.Rest.Response'Class;
                        Stream : in out Servlet.Rest.Output_Stream'Class);

   --  PUT /mon/:id/configure
   procedure Configure (Req    : in out Servlet.Rest.Request'Class;
                        Reply  : in out Servlet.Rest.Response'Class;
                        Stream : in out Servlet.Rest.Output_Stream'Class);

   --  Declare each REST API with a relative URI from Mon_API definition.
   --  GET /api/monitor/:id
   package API_Get_Values is
     new Servlet.Rest.Operation (Handler => Get_Values'Access,
                                 Method  => Servlet.Rest.GET,
                                 URI     => "/api/monitor/:id");

   --  PUT /api/monitor/:id
   package API_Put_Value is
     new Servlet.Rest.Operation (Handler => Put_Value'Access,
                                 Method  => Servlet.Rest.PUT,
                                 URI     => "/api/monitor/:id");

   --  PUT /api/configure
   package API_Configure is
     new Servlet.Rest.Operation (Handler => Configure'Access,
                                 Method  => Servlet.Rest.PUT,
                                 URI     => "/api/monitor/:id/configure");

private

   MAX_VALUES  : constant Natural := 1000;
   MAX_MONITOR : constant Natural := 10;

   type Value_Array is array (Natural range <>) of Natural;

   protected type Monitor_Data is

      procedure Put (Value : in Natural);

      procedure Put (Value : in Natural; Slot : in Natural);

      procedure Configure (Value : in Duration);

      function Get_Values return Value_Array;

   private
      Values      : Value_Array (1 .. MAX_VALUES) := (others => 0);
      Value_Count : Natural := 0;
      Pos         : Natural := 1;
      Slot_Size   : Duration := 10.0;
      Slot_Start  : Ada.Calendar.Time := Ada.Calendar.Clock;
   end Monitor_Data;

end Monitor;
