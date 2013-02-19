-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Bounded queue ADT for sequential use only
-- Each queue has a preset maximum size
--
-- History:
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V2.1--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V2.0--Added Assign; implemented using PragmARC.List_Bounded_Unprotected
-- 2001 Jun 01     J. Carter          V1.1--Added Peek
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.List_Bounded_Unprotected;
generic -- PragmARC.Queue_Bounded_Unprotected
   type Element is private;
package PragmARC.Queue_Bounded_Unprotected is
   pragma Preelaborate;

   type Handle (Max_Size : Positive) is tagged limited private; -- Initial value: emtpy

   procedure Clear (Queue : in out Handle);
   -- Makes Queue empty
   -- Contents of Queue are lost
   -- All queues are initially empty
   --
   -- Time: O(1)
   --
   -- Postcondition: Is_Empty (Queue)

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- Raises Too_Short if To.Max_Size < Length (From)
   -- To is unchanged if Too_Short is Raised
   --
   -- Time: O(N)
   --
   -- Precondition: To.Max_Size >= Length (From)     raise Too_Short if violated

   procedure Put (Into : in out Handle; Item : in Element); -- raise Full
   -- Adds Item to Into
   -- Raises Full if Into is already full
   -- Into is unchanged if Full is raised
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Full (Into)     raise Full if violated
   --
   -- Postcondition: not Is_Empty (Into)

   procedure Get (From : in out Handle; Item : out Element); -- raise Empty
   -- Removes the next Element from From and puts it in Item
   -- Raises Empty if From is empty
   -- From is unchanged if Empty is raised
   -- Contents of Item are undefined if Empty is raised
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Empty (From)     raise Empty if violated
   --
   -- Postcondition: not Is_Full (From)

   function Is_Full (Queue : Handle) return Boolean;
   -- Returns True if Queue is full; False otherwise
   --
   -- Time: O(1)

   function Is_Empty (Queue : Handle) return Boolean;
   -- Returns True if Queue is empty; False otherwise
   --
   -- Time: O(1)

   function Length (Queue : Handle) return Natural;
   -- Returns the number of Elements in Queue
   --
   -- Time: O(N)

   function Peek (Queue : Handle) return Element;
   -- Returnss the Element at the head of Queue without altering Queue
   --
   -- Raises Empty if Queue is empty
   --
   -- Time: O(1)
   --
   -- Precondition: not Is_Empty (Queue)     raise Empty if violated

   generic -- Iterate
      with procedure Action (Item : in out Element; Continue : out Boolean);
   procedure Iterate (Over : in out Handle);
   -- Applies Action to each Element in Over, from head to tail
   -- Iterate terminates immediately if Continue is set to False (remainder of Over is not processed)
private -- PragmARC.Queue_Bounded_Unprotected
   package Implementation is new PragmARC.List_Bounded_Unprotected (Element => Element);

   type Handle (Max_Size : Positive) is tagged limited record
      List : Implementation.Handle (Max_Size => Max_Size);
   end record;
end PragmARC.Queue_Bounded_Unprotected;
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
