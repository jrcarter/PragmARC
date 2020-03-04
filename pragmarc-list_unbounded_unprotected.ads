-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- General purpose list for sequential use
-- A list has elements in sequence; each element has a position in that sequence
-- Positions are used to manipulate a list
--
-- History:
-- 2020 Mar 15     J. Carter          V1.2--Make Next and Prev Object.Operation friendly
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.2--Added Assign
-- 2001 May 01     J. Carter          V1.1--Added Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Finalization;

use Ada;
generic -- PragmARC.List_Unbounded_Unprotected
   type Element is private;
package PragmARC.List_Unbounded_Unprotected is
   pragma Preelaborate;

   type Handle is tagged limited private; -- Initial value: empty

   type Position is private;
   -- A position is initially invalid until assigned a value by First, Last, or Off_List
   -- Other positions accessible via Next and Prev

   Invalid_Position : exception; -- Raised if a position is invalid or if the Off_List position is used for Delete, Get, or Put

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To is unknown if Storage_Exhausted is raised
   --
   -- Time: O(N)

   procedure Clear (List : in out Handle); -- Makes List empty
   -- Time: O(N)

   -- Operations to obtain valid positions for lists:
   function First    (List : Handle) return Position; -- The position of the first item in List
   function Last     (List : Handle) return Position; -- The position of the last  item in List
   function Off_List (List : Handle) return Position;
   -- Next (Last (List), List) and Prev (First (List), List) refer to positions which are "off the list"
   -- This special position is used in those cases
   -- Each list has a unique Off_List position
   -- Next (Last (List), List)  = Off_List (List)
   -- Prev (First (List), List) = Off_List (List)
   --
   -- Next and Prev are supposed to reverse each other
   -- For a valid Position P of Handle L, Next (Prev (P, L), L) = P and Prev (Next (P, L), L) = P
   -- This gives us:
   -- Next (Off_List (List), List) = First (List)
   -- Prev (Off_List (List), List) = Last (List)
   --
   -- A similar relation holds for Insert and Append:
   -- Insert (List, X, Off_List (List), P) <=> Append (List, X, Last (List), P)
   -- Append (List, X, Off_List (List), P) <=> Insert (List, X, First (List), P)
   --
   -- If List is empty, Off_List (List) = First (List) = Last (List)
   -- The Off_List position cannot be used for Delete, Get, or Put
   --
   -- Time: O(1)

   -- Operations to obtain valid positions from valid positions:
   function Next (List : Handle; Pos : Position) return Position; -- raise Invalid_Position
   function Prev (List : Handle; Pos : Position) return Position; -- raise Invalid_Position
   -- Next and Prev raise Invalid_Position if Pos is invalid
   --
   -- Time: O(1)

   -- Operations to manipulate lists
   procedure Insert (Into : in out Handle; Item : in Element; Before : in Position; New_Pos : out Position);
   -- Inserts Item before Before
   -- Returns the position of Item in Into in New_pos
   -- Raises Storage_Exhausted if no more storage is available for Into
   -- Raises Invalid_Position if Pos is invalid
   -- Nothing is changed if Storage_Exhausted or Invalid_Position are raised
   --
   -- Time: O(1)

   procedure Append (Into : in out Handle; Item : in Element; After : in Position; New_Pos : out Position);
   -- Appends Item after After
   -- Returns the position of Item in Into in New_Pos
   -- Raises Storage_Exhausted if no more storage is available for Into
   -- Raises Invalid_Position if Pos is invalid
   -- Nothing is changed if Storage_Exhausted or Invalid_Position are raised
   --
   -- Time: O(1)

   procedure Delete (From : in out Handle; Pos : in out Position); -- raise Invalid_Position
   -- Deletes the item at Pos
   -- Pos is made invalid
   -- Raises Invalid_Position if Pos is invalid or is the Off_List position for From
   -- Nothing is changed if Invalid_Position is raised
   --
   -- Time: O(1)

   function Get (From : Handle; Pos : Position) return Element; -- raise Invalid_Position
   -- Returns the item at Pos
   -- Raises Invalid_Position if Pos is invalid or the Off_List position
   --
   -- Time: O(1)

   procedure Put (Into : in out Handle; Pos : in Position; Item : in Element); -- raise Invalid_Position
   -- Makes the Element stored at Pos be Item
   -- Raises Invalid_Position if Pos is invalid or is the Off_List position for Into
   -- Nothing is changed if Invalid_Position is raised
   --
   -- Time: O(1)

   function Is_Empty (List : Handle) return Boolean;
   -- Returns True if List is empty [Length (List) = 0]; returns False otherwise
   --
   -- Time: O(1)

   function Length (List : Handle) return Natural; -- Returns the number of items in List
   --
   -- Time: O(1)

   generic -- Iterate
      with procedure Action (Item : in out Element; Pos : in Position; Continue : out Boolean);
   procedure Iterate (Over : in out Handle);
   -- Calls Action with each Element in Over, & its Position, in turn
   -- Returns immediately if Continue is set to False (remainder of Over is not processed)

   generic -- Sort
      with function "<" (Left : Element; Right : Element) return Boolean is <>;
   procedure Sort (List : in out Handle); -- raise Storage_Exhausted
   -- Sorts List into ascending order as defined by "<"
   -- Requires one additional list node
   -- Raises Storage_Exhausted if no more storage is available for this extra node
   -- List is unchanged if Storage_Exhausted is raised
   --
   -- Time: O(N log N)
private -- PragmARC.List_Unbounded_Unprotected
   type Node (Has_Data : Boolean);
   type Link is access Node;

   function Initialize return Link;

   type Handle is new Finalization.Limited_Controlled with record
      Off_List : Link    := Initialize;
      Length   : Natural := 0;
   end record;

   procedure Finalize (Object : in out Handle);

   type Position is record
      List_Id : Link := null;
      Ptr     : Link := null;
   end record;
end PragmARC.List_Unbounded_Unprotected;
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
