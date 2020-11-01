-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2000 Dec 01     J. Carter          V1.1--Revised implementation of Iterate
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Queues.Bounded.Protection.Non_Blocking is
   protected body Handle is
      procedure Clear is
         -- Empty
      begin -- Clear
         Queue.Clear;
      end Clear;

      procedure Put (Item : in Element) is
         -- Empty
      begin -- Put
         Queue.Put (Item => Item);
      end Put;

      procedure Get (Item : out Element) is
         -- Empty
      begin -- Get
         Queue.Get (Item => Item);
      end Get;

      function Is_Full return Boolean is
         (Queue.Is_Full);

      function Is_Empty return Boolean is
         (Queue.Is_Empty);

      function Length return Natural is
         (Queue.Length);

      function Peek return Element is
         (Queue.Peek);

      procedure Iterate (Action : access procedure (Item : in Element) ) is
         procedure Local is new Implementation.Iterate (Action => Action.all);
      begin -- Iterate
         Local (Over => Queue);
      end Iterate;
   end Handle;
end PragmARC.Data_Structures.Queues.Bounded.Protection.Non_Blocking;
