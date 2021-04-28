-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- General purpose stack for sequential use
-- This is based on Ada.Containers.Doubly_Linked_Lists and all the restrictions of that apply
--
-- History:
-- 2021 May 01     J. Carter          V2.4--Adhere to coding standard
-- 2021 Feb 01     J. Carter          V2.3--Added postcondition to Pop
-- 2021 Jan 01     J. Carter          V2.2--Removed limited and Assign
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

private with Ada.Containers.Doubly_Linked_Lists;

generic -- PragmARC.Data_Structures.Stacks.Unbounded.Unprotected
   type Element is private;
package PragmARC.Data_Structures.Stacks.Unbounded.Unprotected with Preelaborate is
   type Handle is tagged private; -- Initial value: empty

   procedure Clear (Stack : in out Handle) with
      Post => Stack.Is_Empty;
   -- Makes Stack empty

   procedure Push (Onto : in out Handle; Item : in Element) with
      Post => not Onto.Is_Empty; -- raise Storage_Exhausted
   -- Adds Item to the top of Onto
   -- Raises Storage_Exhausted if no more storage is available for Onto
   -- Nothing is changed if Storage_Exhausted is raised

   procedure Pop (From : in out Handle; Item : out Element) with
      Pre  => not From.Is_Empty or else raise Empty,
      Post => From.Length = From'Old.Length - 1;
   -- Removes the Element at the top of From and assigns it to Item

   function Length (Stack : in Handle) return Natural;
   -- Returns the number of Elements in Stack

   function Is_Empty (Stack : in Handle) return Boolean;
   -- Returns True if Stack is empty; False otherwise

   function Peek (Stack : in Handle) return Element with
      Pre => not Stack.Is_Empty or else raise Empty;
   -- Returns the Element on the top of Stack without altering Stack

   generic -- Iterate
      with procedure Action (Item : in Element);
   procedure Iterate (Over : in out Handle);
   -- Calls Action with each Element in Over in turn, from top to bottom
private -- PragmARC.Data_Structures.Stacks.Unbounded.Unprotected
   package Implementation is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Element);

   type Handle is tagged record
      List : Implementation.List;
   end record;
end PragmARC.Data_Structures.Stacks.Unbounded.Unprotected;
