-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Implements a skip list, a probabilistically-balanced structure similar to a balanced tree in use and in search time
-- Described by W. Pugh in "Skip Lists:  A Probabilistic Alternative to Balanced Trees," CACM 1990 Jun
--
-- History:
-- 2026 Aug 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Data_Structures.Skip_Lists.Bounded
   type Element is private; -- Values to store in the list

   with function "<" (Left : in Element; Right : in Element) return Boolean is <>;
   -- Orders Elements; usually will work on a part (the key) of an Element
   -- Elements will be ordered in ascending order according to "<"

   with function "=" (Left : in Element; Right : in Element) return Boolean is <>;
   -- Usually operates on part (the key) of an Element
package PragmARC.Data_Structures.Skip_Lists.Bounded is
   type Skip_List (Max_Length : Positive) is tagged private; -- Initial value: empty

   procedure Clear (List : in out Skip_List) with
      Post => List.Length = 0;
   -- Makes List empty
   --
   -- Time: O(N)

   procedure Assign (To : in out Skip_List; From : in Skip_List) with
      Pre => From.Length <= To.Max_Length;
   -- Assigns From to To even if they have different Max_Lengths

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
   -- raise Too_Short
   -- Adds Item to List in the order specified by "<"
   -- If there is a value in List which is = Item, replaces the value with Item
   -- Raises Too_Short if there is List is full
   -- List is unchanged if Too_Short is raised
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
   procedure Iterate (List : in Skip_List);
   -- Applies Action to each value in List in order
private -- PragmARC.Skip_List_Unbounded
   Max_Level : constant := 5;

   subtype Level_ID is Positive range 1 .. Max_Level;

   type Forward_Set is array (Level_ID) of Natural;

   type Node is record
      Level   : Level_ID;
      Forward : Forward_Set;
      Value   : Element;
   end record;

   type Node_List is array (Natural range <>) of Node;

   function Initial_List (Length : in Positive) return Node_List;
   -- Creates a list of Length Nodes, with all nodes linked in sequence

   type Skip_List (Max_Length : Positive) is tagged record
      Level  : Level_ID                    := Level_ID'First;
      List   : Node_List (0 .. Max_Length) := Initial_List (Max_Length); -- List (0) is the header
      Last   : Natural                     := 0;
      Length : Natural                     := 0;
      Free   : Natural                     := 1;
   end record;
end PragmARC.Data_Structures.Skip_Lists.Bounded;
