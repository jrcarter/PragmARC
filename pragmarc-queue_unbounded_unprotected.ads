-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- General purpose queue for sequential use
--
-- History:
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.List_Unbounded_Unprotected;
generic -- PragmARC.Queue_Unbounded_Unprotected
   type Element is private;
package PragmARC.Queue_Unbounded_Unprotected is
   pragma Preelaborate;

   type Handle is tagged limited private; -- Initial value: empty

   procedure Clear (Queue : in out Handle);
   -- Makes Queue empty
   --
   -- Time: O(N)
   --
   -- Postcondition: Is_Empty (Queue);

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To is unknown if Storage_Exhausted is raised
   --
   -- Time: O(N)

   procedure Put (Onto : in out Handle; Item : in Element); -- raise Storage_Exhausted
   -- Adds Item to the tail of Onto
   -- Raises Storage_Exhausted if no more storage is available for Onto
   -- Nothing is changed if Storage_Exhausted is raised
   --
   -- Time: O(1)
   --
   -- Postcondition: not Is_Empty (Onto)

   procedure Get (From : in out Handle; Item : out Element); -- raise Empty
   -- Removes the Element at the head of From and assigns it to Item
   -- Raises Empty if From is empty
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Empty (From)     raise Empty if violated
   --
   -- Postcondition: Length (after[From]) = Length (before[From]) - 1

   function Length (Queue : Handle) return Natural;
   -- Returns the number of Elements in Queue
   --
   -- Time: O(1)

   function Is_Empty (Queue : Handle) return Boolean;
   -- Returns True if Queue is empty; False otherwise
   --
   -- Time: O(1)

   function Peek (Queue : Handle) return Element;
   -- Return the Element at the head of Queue without altering Queue
   -- Raises Empty if Queue is empty
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Empty (Queue)     raise Empty if violated

   generic -- Iterate
      with procedure Action (Item : in out Element; Continue : out Boolean);
   procedure Iterate (Over : in out Handle);
   -- Calls Action with each Element in Over in turn, from head to tail
   -- Returns immediately if Continue is set to False (remainder of Over is not processed)
private -- PragmARC.Queue_Unbounded_Unprotected
   package Implementation is new List_Unbounded_Unprotected (Element => Element);

   type Handle is tagged limited record
      List : Implementation.Handle;
   end record;
end PragmARC.Queue_Unbounded_Unprotected;
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
