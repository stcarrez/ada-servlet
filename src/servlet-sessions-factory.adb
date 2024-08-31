-----------------------------------------------------------------------
--  servlet-sessions.factory -- Servlet Sessions factory
--  Copyright (C) 2010, 2011, 2012, 2014, 2015, 2016, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Encoders.Base64;
with Util.Log.Loggers;

--  The <b>Servlet.Sessions.Factory</b> package is a factory for creating, searching
--  and deleting sessions.
package body Servlet.Sessions.Factory is

   use Ada.Finalization;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Servlet.Sessions.Factory");

   --  ------------------------------
   --  Allocate a unique and random session identifier.  The default implementation
   --  generates a 256 bit random number that it serializes as base64 in the string.
   --  Upon successful completion, the sequence string buffer is allocated and
   --  returned in <b>Id</b>.  The buffer will be freed when the session is removed.
   --  ------------------------------
   procedure Allocate_Session_Id (Factory : in out Session_Factory;
                                  Id      : out Ada.Strings.Unbounded.String_Access) is
      use Ada.Streams;

      Rand    : Stream_Element_Array (0 .. 4 * Factory.Id_Size - 1);
      Buffer  : Stream_Element_Array (0 .. 4 * 3 * Factory.Id_Size);
      Encoder : Util.Encoders.Base64.Encoder;
      Last    : Stream_Element_Offset;
      Encoded : Stream_Element_Offset;
   begin
      Factory.Sessions.Generate_Id (Rand);

      --  Encode the random stream in base64 and save it into the Id string.
      Encoder.Transform (Data => Rand, Into => Buffer,
                         Last => Last, Encoded => Encoded);

      Id := new String (1 .. Natural (Encoded + 1));
      for I in 0 .. Encoded loop
         Id (Natural (I + 1)) := Character'Val (Buffer (I));
      end loop;

      Log.Info ("Allocated session {0}", Id.all);
   end Allocate_Session_Id;

   --  ------------------------------
   --  Create a new session
   --  ------------------------------
   procedure Create_Session (Factory : in out Session_Factory;
                             Result  : out Session) is

      Sess    : Session;
      Impl    : constant Session_Record_Access
        := new Session_Record '(Ada.Finalization.Limited_Controlled with
                                Ref_Counter  => Util.Concurrent.Counters.ONE,
                                Create_Time  => Ada.Calendar.Clock,
                                Max_Inactive => Factory.Max_Inactive,
                                others       => <>);
   begin
      Impl.Access_Time := Impl.Create_Time;
      Sess.Impl        := Impl;

      Session_Factory'Class (Factory).Allocate_Session_Id (Impl.Id);

      Factory.Sessions.Insert (Sess);

      Result := Sess;
   end Create_Session;

   --  ------------------------------
   --  Deletes the session.
   --  ------------------------------
   procedure Delete_Session (Factory : in out Session_Factory;
                             Sess    : in out Session) is
   begin
      Factory.Sessions.Delete (Sess);
   end Delete_Session;

   --  ------------------------------
   --  Finds the session knowing the session identifier.
   --  If the session is found, the last access time is updated.
   --  Otherwise, the null session object is returned.
   --  ------------------------------
   procedure Find_Session (Factory : in out Session_Factory;
                           Id      : in String;
                           Result  : out Session) is
   begin
      Result := Factory.Sessions.Find (Id);

      if Result.Is_Valid then
         Result.Impl.Access_Time := Ada.Calendar.Clock;
         Log.Info ("Found active session {0}", Id);
      else
         Log.Info ("Invalid session {0}", Id);
      end if;
   end Find_Session;

   --  ------------------------------
   --  Returns the maximum time interval, in seconds, that the servlet container will
   --  keep this session open between client accesses. After this interval, the servlet
   --  container will invalidate the session. The maximum time interval can be set with
   --  the Set_Max_Inactive_Interval method.
   --  A negative time indicates the session should never timeout.
   --  ------------------------------
   function Get_Max_Inactive_Interval (Factory : in Session_Factory) return Duration is
   begin
      return Factory.Max_Inactive;
   end Get_Max_Inactive_Interval;

   --  ------------------------------
   --  Specifies the time, in seconds, between client requests before the servlet
   --  container will invalidate this session. A negative time indicates the session
   --  should never timeout.
   --  ------------------------------
   procedure Set_Max_Inactive_Interval (Factory  : in out Session_Factory;
                                        Interval : in Duration) is
   begin
      Factory.Max_Inactive := Interval;
   end Set_Max_Inactive_Interval;

   --  ------------------------------
   --  Initialize the session factory.
   --  ------------------------------
   overriding
   procedure Initialize (Factory : in out Session_Factory) is
   begin
      Factory.Sessions.Initialize;
   end Initialize;

   --  ------------------------------
   --  Release all the sessions.
   --  ------------------------------
   overriding
   procedure Finalize (Factory : in out Session_Factory) is
   begin
      Factory.Sessions.Clear;
   end Finalize;

   protected body Session_Cache is

      --  ------------------------------
      --  Find the session in the session cache.
      --  ------------------------------
      function Find (Id : in String) return Session is
         Pos : constant Session_Maps.Cursor := Sessions.Find (Id'Unrestricted_Access);
      begin
         if Session_Maps.Has_Element (Pos) then
            return Session_Maps.Element (Pos);
         else
            return Null_Session;
         end if;
      end Find;

      --  ------------------------------
      --  Insert the session in the session cache.
      --  ------------------------------
      procedure Insert (Sess : in Session) is
      begin
         Sessions.Insert (Sess.Impl.Id.all'Access, Sess);
      end Insert;

      --  ------------------------------
      --  Remove the session from the session cache.
      --  ------------------------------
      procedure Delete (Sess : in out Session) is
         Pos : Session_Maps.Cursor := Sessions.Find (Sess.Impl.Id.all'Access);
      begin
         if Session_Maps.Has_Element (Pos) then
            Session_Maps.Delete (Sessions, Pos);
         end if;
         Finalize (Sess);
      end Delete;

      --  Clear the session cache.
      procedure Clear is
      begin
         null;
      end Clear;

      --  ------------------------------
      --  Generate a random bitstream.
      --  ------------------------------
      procedure Generate_Id (Rand : out Ada.Streams.Stream_Element_Array) is
         use Ada.Streams;
         use Interfaces;

         Size : constant Stream_Element_Offset := Rand'Length / 4;
      begin
         --  Generate the random sequence.
         for I in 0 .. Size - 1 loop
            declare
               Value : constant Unsigned_32 := Id_Random.Random (Random);
            begin
               Rand (4 * I)     := Stream_Element (Value and 16#0FF#);
               Rand (4 * I + 1) := Stream_Element (Shift_Right (Value, 8) and 16#0FF#);
               Rand (4 * I + 2) := Stream_Element (Shift_Right (Value, 16) and 16#0FF#);
               Rand (4 * I + 3) := Stream_Element (Shift_Right (Value, 24) and 16#0FF#);
            end;
         end loop;
      end Generate_Id;

      --  ------------------------------
      --  Initialize the random generator.
      --  ------------------------------
      procedure Initialize is
      begin
         Id_Random.Reset (Random);
      end Initialize;

   end Session_Cache;

end Servlet.Sessions.Factory;
