-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generic unbounded-bag ADT for general use.
--
-- History:
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Size O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
--------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate
-- 2001 Dec 01     J. Carter          V1.2--Added Ceiling_Priority to Handle
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Data_Structures.Bags.Unbounded.Unprotected;

with System;

generic -- PragmARC.Data_Structures.Bags.Unbounded.Protection
   type Element is private;

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Returns True if Left and Right are equal; returns False otherwise.
   -- "=" is often implemented to compare only part of the elements (the Key).
package PragmARC.Data_Structures.Bags.Unbounded.Protection with Preelaborate is
   package Implementation is new PragmARC.Data_Structures.Bags.Unbounded.Unprotected (Element => Element, "=" => "=");

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

      procedure Clear with
         Post => Empty;
      -- Makes the bag empty. All bags are initially empty.

      procedure Add (Item : in Element) with
         Post => not Empty;
      -- Adds Item to the bag.
      -- Raises Storage_Exhausted if we cannot obtain memory to store Item in the bag.
      -- The bag is unchanged if Storage_Exhausted is raised.

      procedure Delete (Item : in Element);
      -- If the bag contains an Element X such that X = Item, deletes X from the bag;
      -- otherwise, has no effect.
      -- If the bag contains more than one such Element, deletes one of these Elements.
      --
      -- Postcondition: before[Length] >= after[Length]

      procedure Update (Item : in Element);
      -- If the bag contains an Element X such that X = Item, performs X := Item;
      -- otherwise, has no effect.
      -- If the bag contains more than one such Element, updates one of these Elements.

      function Find (Key : Element) return Find_Result;
      -- If the bag contains an Element X such that X = Key, returns (Found => True, Item => X);
      -- otherwise, returns (Found => False).
      -- If the bag contains more than one such Element, returns one of these Elements
      -- as the Item component of the result.

      function Empty return Boolean;
      -- Returns True if the bag contains no elements; returns False otherwise

      function Size return Natural;
      -- Returns the number of elements stored in the bag

      procedure Iterate (Action : access procedure (Item : in out Element) );
      -- Applies Action to each Element in the bag in some unspecified order, until every Element in the bag has been processed
   private -- Handle
      Bag : Implementation.Handle;
   end Handle;
end PragmARC.Data_Structures.Bags.Unbounded.Protection;
