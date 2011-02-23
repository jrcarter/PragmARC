-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Implements a skip list, a probabilistically-balanced structure similar to a balanced tree in use and in search time
-- Described by W. Pugh in "Skip Lists:  A Probabilistic Alternative to Balanced Trees," CACM 1990 Jun
--
-- History:
-- 2002 Dec 01     J. Carter          V1.5--Iterate should not allow modification
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 May 01     J. Carter          V1.2--Added Is_Empty; improved memory usage
-- 2000 Jul 01     J. Carter          V1.1--Eliminated function Random from generic specification
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Finalization;

use Ada;
generic -- PragmARC.Skip_List_Unbounded
   type Element is private; -- Values to store in the list

   with function "<" (Left : Element; Right : Element) return Boolean is <>;
   -- Orders Elements; usually will work on a part (the key) of an Element
   -- Elements will be ordered in ascending order according to "<"

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Usually operates on part (the key) of an Element
package PragmARC.Skip_List_Unbounded is
   type Skip_List is limited private; -- Initial value: empty

   procedure Clear (List : in out Skip_List);
   -- Makes List empty
   --
   -- Time: O(N)
   --
   -- Postcondition: Length (List) = 0

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

   function Search (List : Skip_List; Item : Element) return Result;
   -- If there exists a value stored in List such that Value = Item, returns (Found => True, Item => Value)
   -- Returns (Found => False) otherwise
   --
   -- Time: approximately O(log N)

   procedure Insert (List : in out Skip_List; Item : in Element; Duplicates_Allowed : in Boolean := False);
   -- raise Storage_Exhausted
   -- Adds Item to List in the order specified by "<"
   -- If Duplicates_Allowed, adds Item after any values in List which are = Item
   -- If not Duplicates_Allowed and there is a value in List which is = Item, replaces the value with Item
   -- It is recommended that Duplicates_Allowed always be False (see comments for Delete)
   -- Raises Storage_Exhausted if there is insufficient memory available for a list node
   -- List is unchanged if Storage_Exhausted is raised
   --
   -- Time : approximately O(log N)

   procedure Delete (List : in out Skip_List; Item : in Element);
   -- Deletes the first value in List which is = Item
   -- If there is no such value in List, this procedure has no effect
   -- The user is expected to have checked for the existence of such a value before calling this procedure, using Search,
   -- Get_First, or Get_Last
   -- Note that, if duplicates are allowed, Delete may delete a different node than the one found through Search,
   -- Get_First, or Get_Last, with possible unexpected results
   --
   -- Time : approximately O(log N)

   function Get_First (List : Skip_List) return Element; -- raise Empty
   -- Returns the first value stored in List (values are ordered by "<" and by the order in which they are inserted)
   -- Raises Empty if List is empty
   --
   -- Time: O(1)
   --
   -- Precondition : not Is_Empty (List)     raise Empty if violated

   function Get_Last (List : Skip_List) return Element; -- raise Empty
   -- Similar to Get_First except it returns the last value stored in List
   -- Raises Empty if List is empty
   --
   -- Time: O(1)
   --
   -- Precondition : not Is_Empty (List)     raise Empty if violated

   function Is_Empty (List : Skip_List) return Boolean;
   -- Returns True if List is empty [Length (List) = 0]; returns False otherwise
   --
   -- Time : O(1)

   function Length (List : Skip_List) return Natural;
   -- Returns a count of the number of values stored in List
   --
   -- Time : O(N)

   generic -- Iterate
      type Context_Data (<>) is limited private;

      with procedure Action (Item : in Element; Context : in out Context_Data; Continue : out Boolean);
   procedure Iterate (List : in out Skip_List; Context : in out Context_Data);
   -- Applies Action to each value in List in order
   -- Returns immediately if Action sets Continue to False
private -- PragmARC.Skip_List_Unbounded
   Max_Level : constant := 5;

   subtype Level_Id is Positive range 1 .. Max_Level;

   type Node (Has_Data : Boolean; Level : Level_Id);
   type Link is access all Node;

   type Forward_Set is array (Level_Id range <>) of Link;

   type Node (Has_Data : Boolean; Level : Level_Id) is record
      Forward : Forward_Set (Level_Id'First .. Level) := Forward_Set'(Level_Id'First .. Level => null);

      case Has_Data is
      when False =>
         null;
      when True =>
         Value : Element;
      end case;
   end record;

   type Skip_List is new Finalization.Limited_Controlled with record
      Level  : Level_Id := Level_Id'First;
      Header : Link     := new Node (Has_Data => False, Level => Max_Level);
      Last   : Link     := null;
   end record;

   procedure Finalize (Object : in out Skip_List);
end PragmARC.Skip_List_Unbounded;
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