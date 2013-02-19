-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Bounded queue ADT for concurrent use only
-- Each queue has a preset maximum size
-- A call to Put when the queue is full  blocks the caller until another task calls Get
-- A call to Get when the queue is empty blocks the caller until another task calls Put
--
-- History:
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.2--Added Ceiling_Priority to Handle
-- 2001 Jun 01     J. Carter          V1.1--Added Peek
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Queue_Bounded_Unprotected;

with System;
generic -- PragmARC.Queue_Bounded_Blocking
   type Element is private;
package PragmARC.Queue_Bounded_Blocking is
   pragma Preelaborate;

   package Implementation is new PragmARC.Queue_Bounded_Unprotected (Element => Element);

   protected type Handle (Max_Size : Positive; Ceiling_Priority : System.Any_Priority) is
      pragma Priority (Ceiling_Priority);

      procedure Clear;
      -- Empties the queue; anything on the queue is lost; the queue is initially empty
      --
      -- Time: O(1)

      entry Put (Item : in Element);
      -- Adds Item to the tail of the queue
      -- If the queue is full, the caller is blocked until another task calls Get
      --
      -- Time: O(1)
      --
      -- Barrier:       not Is_Full
      --
      -- Postcondition: not Is_Empty

      entry Get (Item : out Element);
      -- Removes the next Element from the head of the queue and returns it in Item
      -- If the queue is empty, the caller is blocked until another task calls Put
      --
      -- Time: O(1)
      --
      -- Barrier:       not Is_Empty
      --
      -- Postcondition: not Is_Full

      function Full return Boolean;
      -- Returns True if the queue is full; False otherwise
      --
      -- Time: O(1)

      function Empty return Boolean;
      -- Returns True if the queue is empty; False otherwise
      --
      -- Time: O(1)

      function Length return Natural;
      -- Returns the number of Elements in the queue
      --
      -- Time: O(1)

      function Peek return Element;
      -- Returns the Element at the head of the queue without altering the queue
      -- Raises Empty if the queue is empty
      --
      -- Time: O(1)
      --
      -- Precondition:  not Is_Empty     raise Empty if violated

      procedure Iterate (Action : access procedure (Item : in out Element; Continue : out Boolean) );
      -- Applies Action to each Element in the queue in turn, from head to tail.
      -- Iterate returns immediately if Action sets Continue to False (remainder of the queue is not processed)
   private -- Handle
      Queue : Implementation.Handle (Max_Size);
   end Handle;
end PragmARC.Queue_Bounded_Blocking;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
