-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic unbounded-bag ADT for sequential use only.
--
-- History:
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.2--Added Assign
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.List_Unbounded_Unprotected;
generic -- PragmARC.Bag_Unbounded_Unprotected
   type Element is private;

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Returns True if Left and Right are equal; returns False otherwise.
   -- "=" is often implemented to compare only part of the elements (the Key).
package PragmARC.Bag_Unbounded_Unprotected is
   pragma Preelaborate;

   type Handle is tagged limited private; -- Intial value: Empty

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To is undefined if Storage_Exhausted is raised
   --
   -- Time: O(N)

   procedure Clear (Bag : in out Handle);
   -- Makes Bag empty. All bags are initially empty.
   --
   -- Time: O(N)
   --
   -- Postcondition: Empty (Bag)

   procedure Add (Item : in Element; Into : in out Handle);
   -- Adds Item to Into.
   -- Raises Storage_Exhausted if we cannot obtain memory to store Item in Into.
   -- Into is unchanged if Storage_Exhausted is raised
   -- Time: O(1)
   --
   -- Postcondition: not Empty (Into)

   procedure Delete (Item : in Element; From : in out Handle);
   -- If From contains an Element X such that X = Item, deletes X from From;
   -- otherwise, has no effect.
   -- If From contains more than one such Element, deletes one of these Elements.
   --
   -- Time: O(N)
   --
   -- Postcondition: Length (before[From]) >= Length (after[From])

   procedure Update (Item : in Element; Bag : in out Handle);
   -- If Bag contains an Element X such that X = Item, performs X := Item;
   -- otherwise, has no effect.
   -- If Bag contains more than one such Element, updates one of these Elements.
   --
   -- Time: O(N)

   type Find_Result (Found : Boolean := False) is record
      case Found is
      when False =>
         null;
      when True =>
         Item : Element;
      end case;
   end record;

   function Find (Key : Element; Bag : Handle) return Find_Result;
   -- If Bag contains an Element X such that X = Key, returns (Found => True, Item => X);
   -- otherwise, returns (Found => False).
   -- If Bag contains more than one such Element, returns one of these Elements
   -- as the Item component of the result.
   --
   -- Time: O(N)

   function Empty (Bag : Handle) return Boolean;
   -- Returns True if Bag contains no elements; returns False otherwise
   --
   -- Time: O(1)

   function Size (Bag : Handle) return Natural;
   -- Returns the number of elements stored in Bag
   --
   -- Time: O(N)

   generic -- Iterate
      with procedure Action (Item : in out Element; Continue : out Boolean);
   procedure Iterate (Over : in out Handle);
   -- Applies Action to each Element in the bag in some unspecified order, until either
   -- 1.  Action sets Continue to False, or
   -- 2.  Every Element in the bag has been processed
private -- PragmARC.Bag_Unbounded_Unprotected
   package Implementation is new PragmARC.List_Unbounded_Unprotected (Element => Element);

   type Handle is tagged limited record
      List : Implementation.Handle;
   end record;
end PragmARC.Bag_Unbounded_Unprotected;
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
