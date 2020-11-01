-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generic unbounded-bag ADT for sequential use only.
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Feb 15     J. Carter          V1.2--Make more Object.Operation friendly
-- 2018 Aug 01     J. Carter          V1.1--Make Size O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.2--Added Assign
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Doubly_Linked_Lists;

generic -- PragmARC.Data_Structures.Bags.Unbounded.Unprotected
   type Element is private;

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Returns True if Left and Right are equal; returns False otherwise.
   -- "=" is often implemented to compare only part of the elements (the Key).
package PragmARC.Data_Structures.Bags.Unbounded.Unprotected is
   pragma Preelaborate;

   type Handle is tagged limited private; -- Intial value: Empty

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To is undefined if Storage_Exhausted is raised
   --
   -- Time: O(N)

   procedure Clear (Bag : in out Handle) with
      Post => Bag.Empty;
   -- Makes Bag empty. All bags are initially empty.
   --
   -- Time: O(N)

   procedure Add (Into : in out Handle; Item : in Element) with
      Post => not Into.Empty;
   -- Adds Item to Into.
   -- Raises Storage_Exhausted if we cannot obtain memory to store Item in Into.
   -- Into is unchanged if Storage_Exhausted is raised
   -- Time: O(1)

   procedure Delete (From : in out Handle; Item : in Element);
      -- with Post => From'Old.Size >= From.Size; -- 'Old cannot be used with a limited type
   -- If From contains an Element X such that X = Item, deletes X from From;
   -- otherwise, has no effect.
   -- If From contains more than one such Element, deletes one of these Elements.
   --
   -- Time: O(N)

   procedure Update (Bag : in out Handle; Item : in Element);
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

   function Find (Bag : Handle; Key : Element) return Find_Result;
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
   -- Time: O(1)

   generic -- Iterate
      with procedure Action (Item : in out Element);
   procedure Iterate (Over : in out Handle);
   -- Applies Action to each Element in Over in some unspecified order, until every Element in Over has been processed
private -- PragmARC.Bag_Unbounded_Unprotected
   package Implementation is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Element);

   type Handle is tagged limited record
      List : Implementation.List;
   end record;
end PragmARC.Data_Structures.Bags.Unbounded.Unprotected;
