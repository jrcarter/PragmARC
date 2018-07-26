-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic unbounded-bag ADT for general use.
--
-- History:
-- 2018 Aug 01     J. Carter          V1.1--Make Size O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
--------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate
-- 2001 Dec 01     J. Carter          V1.2--Added Ceiling_Priority to Handle
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Bag_Unbounded_Unprotected;

with System;
generic -- PragmARC.Bag_Unbounded
   type Element is private;

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Returns True if Left and Right are equal; returns False otherwise.
   -- "=" is often implemented to compare only part of the elements (the Key).
package PragmARC.Bag_Unbounded is
   pragma Preelaborate;

   package Implementation is new PragmARC.Bag_Unbounded_Unprotected (Element => Element, "=" => "=");

   type Find_Result (Found : Boolean := False) is record -- Type returned by Find
      case Found is
      when False =>
         null;
      when True =>
         Item : Element;
      end case;
   end record;

   protected type Handle (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is
      pragma Priority (Ceiling_Priority);

      procedure Clear;
      -- Makes the bag empty. All bags are initially empty.
      --
      -- Time: O(N)
      --
      -- Postcondition: Empty

      procedure Add (Item : in Element);
      -- Adds Item to the bag.
      -- Raises Storage_Exhausted if we cannot obtain memory to store Item in the bag.
      -- The bag is unchanged if Storage_Exhausted is raised.
      --
      -- Time: O(1)
      --
      -- Postcondition: not Empty

      procedure Delete (Item : in Element);
      -- If the bag contains an Element X such that X = Item, deletes X from the bag;
      -- otherwise, has no effect.
      -- If the bag contains more than one such Element, deletes one of these Elements.
      --
      -- Time: O(N)
      --
      -- Postcondition: before[Length] >= after[Length]

      procedure Update (Item : in Element);
      -- If the bag contains an Element X such that X = Item, performs X := Item;
      -- otherwise, has no effect.
      -- If the bag contains more than one such Element, updates one of these Elements.
      --
      -- Time: O(N)

      function Find (Key : Element) return Find_Result;
      -- If the bag contains an Element X such that X = Key, returns (Found => True, Item => X);
      -- otherwise, returns (Found => False).
      -- If the bag contains more than one such Element, returns one of these Elements
      -- as the Item component of the result.
      --
      -- Time: O(N)

      function Empty return Boolean;
      -- Returns True if the bag contains no elements; returns False otherwise
      --
      -- Time: O(1)

      function Size return Natural;
      -- Returns the number of elements stored in the bag
      --
      -- Time: O(1)

      procedure Iterate (Action : access procedure (Item : in out Element; Continue : out Boolean) );
      -- Applies Action to each Element in the bag in some unspecified order, until either
      -- 1.  Action sets Continue to False, or
      -- 2.  Every Element in the bag has been processed
   private -- Handle
      Bag : Implementation.Handle;
   end Handle;
end PragmARC.Bag_Unbounded;
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
