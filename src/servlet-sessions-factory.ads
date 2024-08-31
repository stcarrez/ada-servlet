-----------------------------------------------------------------------
--  servlet-sessions.factory -- Servlet Sessions factory
--  Copyright (C) 2010, 2011, 2014, 2016, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
with Util.Strings;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Discrete_Random;
with Interfaces;
with Ada.Streams;

--  The <b>Servlet.Sessions.Factory</b> package is a factory for creating, searching
--  and deleting sessions.
package Servlet.Sessions.Factory is

   type Session_Factory is new Ada.Finalization.Limited_Controlled with private;

   --  Create a new session
   procedure Create_Session (Factory : in out Session_Factory;
                             Result  : out Session);

   --  Allocate a unique and random session identifier.  The default implementation
   --  generates a 256 bit random number that it serializes as base64 in the string.
   --  Upon successful completion, the sequence string buffer is allocated and
   --  returned in <b>Id</b>.  The buffer will be freed when the session is removed.
   procedure Allocate_Session_Id (Factory : in out Session_Factory;
                                  Id      : out Ada.Strings.Unbounded.String_Access);

   --  Deletes the session.
   procedure Delete_Session (Factory : in out Session_Factory;
                             Sess    : in out Session);

   --  Finds the session knowing the session identifier.
   --  If the session is found, the last access time is updated.
   --  Otherwise, the null session object is returned.
   procedure Find_Session (Factory : in out Session_Factory;
                           Id      : in String;
                           Result  : out Session);

   --  Returns the maximum time interval, in seconds, that the servlet container will
   --  keep this session open between client accesses. After this interval, the servlet
   --  container will invalidate the session. The maximum time interval can be set with
   --  the Set_Max_Inactive_Interval method.
   --  A negative time indicates the session should never timeout.
   function Get_Max_Inactive_Interval (Factory : in Session_Factory) return Duration;

   --  Specifies the time, in seconds, between client requests before the servlet
   --  container will invalidate this session. A negative time indicates the session
   --  should never timeout.
   procedure Set_Max_Inactive_Interval (Factory  : in out Session_Factory;
                                        Interval : in Duration);

   --  Release all the sessions.
   overriding
   procedure Finalize (Factory : in out Session_Factory);

private

   use Util.Strings;

   package Session_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Name_Access,
                                 Element_Type    => Session,
                                 Hash            => Hash,
                                 Equivalent_Keys => Util.Strings.Equivalent_Keys);

   package Id_Random is new Ada.Numerics.Discrete_Random (Interfaces.Unsigned_32);

   protected type Session_Cache is

      --  Find the session in the session cache.
      function Find (Id : in String) return Session;

      --  Insert the session in the session cache.
      procedure Insert (Sess : in Session);

      --  Remove the session from the session cache.
      procedure Delete (Sess : in out Session);

      --  Generate a random bitstream.
      procedure Generate_Id (Rand : out Ada.Streams.Stream_Element_Array);

      --  Initialize the random generator.
      procedure Initialize;

      --  Clear the session cache.
      procedure Clear;
   private
      --  Id to session map.
      Sessions     : Session_Maps.Map;

      --  Random number generator used for ID generation.
      Random       : Id_Random.Generator;
   end Session_Cache;

   type Session_Factory is new Ada.Finalization.Limited_Controlled with record
      --  The session cache.
      Sessions     : Session_Cache;

      --  Max inactive time in seconds.
      Max_Inactive : Duration := DEFAULT_INACTIVE_TIMEOUT;

      --  Number of 32-bit random numbers used for the ID generation.
      Id_Size      : Ada.Streams.Stream_Element_Offset := 8;
   end record;

   --  Initialize the session factory.
   overriding
   procedure Initialize (Factory : in out Session_Factory);

end Servlet.Sessions.Factory;
