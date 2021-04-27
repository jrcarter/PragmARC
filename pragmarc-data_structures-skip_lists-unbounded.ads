-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implements a skip list, a probabilistically-balanced structure similar to a balanced tree in use and in search time
-- Described by W. Pugh in "Skip Lists:  A Probabilistic Alternative to Balanced Trees," CACM 1990 Jun
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------
-- 2002 Dec 01     J. Carter          V1.5--Iterate should not allow modification
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 May 01     J. Carter          V1.2--Added Is_Empty; improved memory usage
-- 2000 Jul 01     J. Carter          V1.1--Eliminated function Random from generic specification
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

private with Ada.Finalization;

generic -- PragmARC.Data_Structures.Skip_Lists.Unbounded
   type Element is private; -- Values to store in the list

   with function "<" (Left : in Element; Right : in Element) return Boolean is <>;
   -- Orders Elements; usually will work on a part (the key) of an Element
   -- Elements will be ordered in ascending order according to "<"

   with function "=" (Left : in Element; Right : in Element) return Boolean is <>;
   -- Usually operates on part (the key) of an Element
package PragmARC.Data_Structures.Skip_Lists.Unbounded is
   type Skip_List is tagged limited private; -- Initial value: empty

   procedure Clear (List : in out Skip_List) with
      Post => List.Length = 0;
   -- Makes List empty
   --
   -- Time: O(N)

   procedure Assign (To : out Skip_List; From : in Skip_List);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To if Storage_Exhausted is raised is undefined
   --
   -- Time: O(N)

   type Result (Found : Boolean := False) is record
      case Found is
      when False =>
         null;
      when True =>
         Item : Element;
      end case;
   end record;

   function Search (List : in Skip_List; Item : in Element) return Result;
   -- If there exists a value stored in List such that Value = Item, returns (Found => True, Item => Value)
   -- Returns (Found => False) otherwise
   --
   -- Time: approximately O(log N)

   procedure Insert (List : in out Skip_List; Item : in Element) with
      Post => List.Search (Item).Found;
   -- raise Storage_Exhausted
   -- Adds Item to List in the order specified by "<"
   -- If there is a value in List which is = Item, replaces the value with Item
   -- Raises Storage_Exhausted if there is insufficient memory available for a list node
   -- List is unchanged if Storage_Exhausted is raised
   --
   -- Time : approximately O(log N)

   procedure Delete (List : in out Skip_List; Item : in Element);
   -- Deletes the value in List which is = Item
   -- If there is no such value in List, this procedure has no effect
   --
   -- Time : approximately O(log N)

   function Get_First (List : in Skip_List) return Element with
      Pre => not List.Is_Empty or else raise Empty;
   -- Returns the first value stored in List (values are ordered by "<")
   --
   -- Time: O(1)

   function Get_Last (List : in Skip_List) return Element with
      Pre => not List.Is_Empty or else raise Empty;
   -- Similar to Get_First except it returns the last value stored in List
   --
   -- Time: O(1)

   function Is_Empty (List : in Skip_List) return Boolean;
   -- Returns True if List is empty [List.Length = 0]; returns False otherwise
   --
   -- Time : O(1)

   function Length (List : in Skip_List) return Natural;
   -- Returns a count of the number of values stored in List
   --
   -- Time : O(1)

   generic -- Iterate
      with procedure Action (Item : in Element);
   procedure Iterate (List : in out Skip_List);
   -- Applies Action to each value in List in order
private -- PragmARC.Skip_List_Unbounded
   Max_Level : constant := 5;

   subtype Level_Id is Positive range 1 .. Max_Level;

   type Node (Has_Data : Boolean; Level : Level_Id);
   type Link is access all Node;

   type Forward_Set is array (Level_Id range <>) of Link;

   type Node (Has_Data : Boolean; Level : Level_Id) is record
      Forward : Forward_Set (Level_Id'First .. Level);

      case Has_Data is
      when False =>
         null;
      when True =>
         Value : Element;
      end case;
   end record;

   type Skip_List is new Ada.Finalization.Limited_Controlled with record
      Level  : Level_Id := Level_Id'First;
      Header : Link     := new Node (Has_Data => False, Level => Max_Level);
      Last   : Link     := null;
      Length : Natural  := 0;
   end record;

   procedure Finalize (Object : in out Skip_List);
end PragmARC.Data_Structures.Skip_Lists.Unbounded;
