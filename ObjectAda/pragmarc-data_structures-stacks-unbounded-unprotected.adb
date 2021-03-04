-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 Jan 01     J. Carter          V2.1--Removed Assign
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Stacks.Unbounded.Unprotected is
   -- Conventions: The top    of a stack is the first Element in its List
   --              The bottom of a stack is the last  Element in its List

   procedure Clear (Stack : in out Handle) is
      -- Empty
   begin -- Clear
      Stack.List.Clear;
   end Clear;

   function Is_Empty (Stack : Handle) return Boolean is
      (Stack.List.Is_Empty);

   procedure Iterate (Over : in out Handle) is
      procedure Action (Pos : in Implementation.Cursor) is
         -- Empty
      begin -- Action
         Action (Item => Implementation.Element (Pos) );
      end Action;
   begin -- Iterate
      Over.List.Iterate (Process => Action'Access);
   end Iterate;

   function Length (Stack : Handle) return Natural is
      (Integer (Stack.List.Length) );

   function Peek (Stack : Handle) return Element is
      (Stack.List.First_Element);

   procedure Pop (From : in out Handle; Item : out Element) is
      -- Empty
   begin -- Pop
      Item := From.List.First_Element;
      From.List.Delete_First;
   end Pop;

   procedure Push (Onto : in out Handle; Item : in Element) is
      -- Empty
   begin -- Push
      Onto.List.Prepend (New_Item => Item);
   exception -- Push
   when Storage_Error =>
      raise Storage_Exhausted;
   end Push;
end PragmARC.Data_Structures.Stacks.Unbounded.Unprotected;
