-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Skip lists that are stored in files
--
-- History:
-- 2014 Oct 01     J. Carter          V1.0--Initial version
--
private with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with PragmARC.Skip_List_Unbounded;

generic -- PragmARC.Persistent_Skip_List_Unbounded
   type Element is private; -- Values to store in the list

   with function "<" (Left : Element; Right : Element) return Boolean is <>;
   -- Orders Elements; usually will work on a part (the key) of an Element
   -- Elements will be ordered in ascending order according to "<"

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   -- Usually operates on part (the key) of an Element
package PragmARC.Persistent_Skip_List_Unbounded is
   type Persistent_Skip_List (<>) is tagged limited private;

   Invalid_File : exception;

   function Open_List (Filename : in String; Write_On_Modify : in Boolean := False) return Persistent_Skip_List;
   -- If a file named Filename exists, opens it and reads it into a Persistent_Skip_List, which is returned
   -- Otherwise, creates an empty file named Filename and returns an empty Persistent_Skip_List
   -- Raises Invalid_File if Filename exists but does not represent a valid Persistent_Skip_List
   -- if Write_On_Modify, the list will be written to Filename whenever it is modified (safer but slower)
   -- Otherwise, it will be written when finalized (Faster but less safe)

   procedure Clear (List : in out Persistent_Skip_List);
   -- Makes List empty
   --
   -- Time: O(N)
   --
   -- Postcondition: Length (List) = 0

   type Result (Found : Boolean := False) is record
      case Found is
      when False =>
         null;
      when True =>
         Item : Element;
      end case;
   end record;

   function Search (List : Persistent_Skip_List; Item : Element) return Result;
   -- If there exists a value stored in List such that Value = Item, returns (Found => True, Item => Value)
   -- Returns (Found => False) otherwise
   --
   -- Time: approximately O(log N)

   procedure Insert (List : in out Persistent_Skip_List; Item : in Element);
   -- raise Storage_Exhausted
   -- Adds Item to List in the order specified by "<"
   -- If there is a value in List which is = Item, replaces the value with Item
   -- Raises Storage_Exhausted if there is insufficient memory available for a list node
   -- List is unchanged if Storage_Exhausted is raised
   --
   -- Time : approximately O(log N)

   procedure Delete (List : in out Persistent_Skip_List; Item : in Element);
   -- Deletes the first value in List which is = Item
   -- If there is no such value in List, this procedure has no effect
   --
   -- Time : approximately O(log N)

   function Get_First (List : Persistent_Skip_List) return Element; -- raise Empty
   -- Returns the first value stored in List (values are ordered by "<")
   -- Raises Empty if List is empty
   --
   -- Time: O(1)
   --
   -- Precondition : not Is_Empty (List)     raise Empty if violated

   function Get_Last (List : Persistent_Skip_List) return Element; -- raise Empty
   -- Similar to Get_First except it returns the last value stored in List
   -- Raises Empty if List is empty
   --
   -- Time: O(1)
   --
   -- Precondition : not Is_Empty (List)     raise Empty if violated

   function Is_Empty (List : Persistent_Skip_List) return Boolean;
   -- Returns True if List is empty [Length (List) = 0]; returns False otherwise
   --
   -- Time : O(1)

   function Length (List : Persistent_Skip_List) return Natural;
   -- Returns a count of the number of values stored in List
   --
   -- Time : O(N)

   generic -- Iterate
      with procedure Action (Item : in Element; Continue : out Boolean);
   procedure Iterate (List : in out Persistent_Skip_List);
   -- Applies Action to each value in List in order
   -- Returns immediately if Action sets Continue to False
private -- Persistent_Skip_List_Unbounded
   package Lists is new PragmARC.Skip_List_Unbounded (Element => Element);

   type Persistent_Skip_List is new Ada.Finalization.Limited_Controlled with record
      Filename        : Ada.Strings.Unbounded.Unbounded_String;
      Write_On_Modify : Boolean;
      List            : Lists.Skip_List;
      Finalized       : Boolean := False;
   end record;

   procedure Finalize (Object : in out Persistent_Skip_List);
end PragmARC.Persistent_Skip_List_Unbounded;
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
