-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Bounded queue ADT for general use
-- Each queue has a preset maximum size
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
generic -- PragmARC.Queue_Bounded
   type Element is private;
package PragmARC.Queue_Bounded is
   pragma Preelaborate;

   package Implementation is new PragmARC.Queue_Bounded_Unprotected (Element => Element);

   protected type Handle (Max_Size : Positive; Ceiling_Priority : System.Any_Priority) is
   -- Initial value: emtpy
      pragma Priority (Ceiling_Priority);

      procedure Clear;
      -- Makes the queue empty
      -- Contents of the queue are lost
      -- The queue is initially empty
      --
      -- Time: O(1)
      --
      -- Postcondition: Is_Empty

      procedure Put (Item : in Element); -- raise Full
      -- Adds Item to the queue
      -- Raises Full the queue is already full
      -- The queue is unchanged if Full is raised
      --
      -- Time: O(1)
      --
      -- Precondition:  not Is_Full     raise Full if violated
      --
      -- Postcondition: not Is_Empty

      procedure Get (Item : out Element); -- raise Empty
      -- Removes the next Element from the queue and puts it in Item
      -- Raises Empty if the queue is empty
      -- The queue is unchanged if Empty is raised
      -- Contents of Item are undefined if Empty is raised
      --
      -- Time: O(1)
      --
      -- Precondition:  not Is_Empty     raise Empty if violated
      --
      -- Postcondition: not Is_Full

      function Is_Full return Boolean;
      -- Returns True if the queue is full; False otherwise
      --
      -- Time: O(1)

      function Is_Empty return Boolean;
      -- Returns True if the queue is empty; False otherwise
      --
      -- Time: O(1)

      function Length return Natural; -- Returns the number of Elements in the queue
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
      -- Iterate returns immediately if Action sets Continue to False (remainder of queue is not processed)
   private -- Handle
      Queue : Implementation.Handle (Max_Size => Max_Size);
   end Handle;
end PragmARC.Queue_Bounded;
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
