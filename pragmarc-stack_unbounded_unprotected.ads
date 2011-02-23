-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- General purpose stack for sequential use
--
-- History:
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.List_Unbounded_Unprotected;
generic -- PragmARC.Stack_Unbounded_Unprotected
   type Element is limited private;

   with procedure Assign (To : out Element; From : in Element) is <>;
package PragmARC.Stack_Unbounded_Unprotected is
   pragma Preelaborate;

   type Handle is limited private; -- Initial value: empty

   procedure Clear (Stack : in out Handle);
   -- Makes Stack empty
   --
   -- Time: O(N)
   --
   -- Postcondition: Is_Empty (Stack);

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To is unknown if Storage_Exhausted is raised
   --
   -- Time: O(N)

   procedure Push (Onto : in out Handle; Item : in Element); -- raise Storage_Exhausted
   -- Adds Item to the top of Onto
   -- Raises Storage_Exhausted if no more storage is available for Onto
   -- Nothing is changed if Storage_Exhausted is raised
   --
   -- Time: O(1)
   --
   -- Postcondition: not Is_Empty (Onto)

   procedure Pop (From : in out Handle; Item : out Element); -- raise Empty
   -- Removes the Element at the top of From and assigns it to Item
   -- Raises Empty if From is empty
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Empty (From)     raise Empty if violated
   --
   -- Postcondition: Length (after[From]) = Length (before[From]) - 1

   function Length (Stack : Handle) return Natural;
   -- Returns a count of the number of Elements in Stack
   --
   -- Time: O(N)

   function Is_Empty (Stack : Handle) return Boolean;
   -- Returns True if Stack is empty; False otherwise
   --
   -- Time: O(1)

   function Peek (Stack : Handle) return Element;
   -- Returns the Element on the top of Stack without altering Stack
   -- Raises Empty if Stack is empty
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Empty (Stack)     raise Empty if violated

   generic -- Iterate
      type Context_Data (<>) is limited private;

      with procedure Action (Item : in out Element; Context : in out Context_Data; Continue : out Boolean);
   procedure Iterate (Over : in out Handle; Context : in out Context_Data);
   -- Calls Action with each Element in Over in turn, from top to bottom
   -- Returns immediately if Continue is set to False (remainder of Over is not processed)
private -- PragmARC.Stack_Unbounded_Unprotected
   package Implementation is new List_Unbounded_Unprotected (Element => Element, Assign => Assign);

   type Handle is record
      List : Implementation.Handle;
   end record;
end PragmARC.Stack_Unbounded_Unprotected;
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