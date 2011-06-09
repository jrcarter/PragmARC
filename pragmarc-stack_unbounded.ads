-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- General purpose stack for general use
--
-- History:
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.3--Added Ceiling_Priority to Handle
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Stack_Unbounded_Unprotected;

with System;
generic -- PragmARC.Stack_Unbounded
   type Element is limited private;

   with procedure Assign (To : out Element; From : in Element) is <>;
package PragmARC.Stack_Unbounded is
   pragma Preelaborate;

   package Implementation is new Stack_Unbounded_Unprotected (Element => Element, Assign => Assign);

   type Context_Data is tagged null record; -- Provides context data to Iterate

   type Action_Ptr is access procedure (Item : in out Element; Context : in out Context_Data'Class; Continue : out Boolean);
   -- We can't have a generic protected subprogram, so we use this type to implement Iterate. This means that
   -- the actual procedure passed to Iterate must be declared at the library level to pass accessibility checks

   protected type Handle (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is -- Initial value: empty
      pragma Priority (Ceiling_Priority);

      procedure Clear;
      -- Makes the stack empty
      --
      -- Time: O(N)
      --
      -- Postcondition: Is_Empty

      procedure Push (Item : in Element); -- raise Storage_Exhausted
      -- Adds Item to the top of the stack
      -- Raises Storage_Exhausted if there is insufficient storage for the Element
      -- The stack is unchanged if Storage_Exhausted is raised
      --
      -- Time: O(1)
      --
      -- Postcondition: not Is_Empty

      procedure Pop (Item : out Element); -- raise Empty
      -- Removes the top Element from the stack and puts it in Item
      -- Raises Empty if the stack has no elements
      -- The stack is unchanged if Empty is raised
      -- Contents of Item are undefined if Empty is raised
      --
      -- Time: O(1)
      --
      -- Precondition:  not Is_Empty     raises Empty if violated
      --
      -- Postcondition: after[Length] = before[Length] - 1

      function Is_Empty return Boolean;
      -- Returns True if the stack is empty; False otherwise
      --
      -- Time: O(1)

      function Length return Natural;
      -- Returns the number of Elements in the stack
      --
      -- Time: O(N)

      function Peek return Element;
      -- Returns the Element at the top of the stack without altering the stack
      -- Raises Empty if the stack is empty
      --
      -- Time: O(1)
      --
      -- Precondition:  not Is_Empty     raises Empty if violated

      procedure Iterate (Action : in Action_Ptr; Context : in out Context_Data'Class);
      -- Applies Action to each Element in the stack in turn, from top to bottom
      -- Iterate returns immediately if Action sets Continue to False
   private -- Handle
      Stack : Implementation.Handle;
   end Handle;
end PragmARC.Stack_Unbounded;
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