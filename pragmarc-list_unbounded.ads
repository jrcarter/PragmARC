-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- General purpose list for general use
-- A list has elements in sequence; each element has a position in that sequence
-- Positions are used to manipulate a list
--
-- History:
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.2--Added Ceiling_Priority to Handle
-- 2001 May 01     J. Carter          V1.1--Added Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.List_Unbounded_Unprotected;

with System;
generic -- PragmARC.List_Unbounded
   type Element is private;
package PragmARC.List_Unbounded is
   pragma Preelaborate;

   package Implementation is new PragmARC.List_Unbounded_Unprotected (Element => Element);

   type Position is private;
   -- A position is initially invalid until assigned a value by First, Last, or Off_List
   -- Other positions accessible via Next and Prev

   Invalid_Position : exception; -- Raised if a position is invalid or if the Off_List position is used for Delete, Get, or Put

   protected type Handle (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is -- Initial value: Empty
      pragma Priority (Ceiling_Priority);

      procedure Clear; -- Makes the list empty

      -- Operations to obtain valid positions for lists:
      function First    return Position; -- The position of the first item in the list
      function Last     return Position; -- The position of the last  item in the list
      function Off_List return Position;
      -- List.Next (List.Last) and List.Prev (List.First) refer to positions which are "off the list"
      -- This special position is used in those cases
      -- Each list has a unique Off_List position
      -- List.Next (List.Last)  = List.Off_List
      -- List.Prev (List.First) = List.Off_List
      --
      -- Next and Prev are supposed to reverse each other
      -- For a valid Position P of Handle L, L.Next (L.Prev (P) ) = P and L.Prev (L.Next (P) ) = P
      -- This gives us:
      -- List.Next (List.Off_List) = List.First
      -- List.Prev (List.Off_List) = List.Last
      --
      -- A similar relation holds for Insert and Append:
      -- List.Insert (X, List.Off_List) <=> List.Append (X, List.Last)
      -- List.Append (X, List.Off_List) <=> List.Insert (X, List.First)
      --
      -- If List is empty, List.Off_List = List.First = List.Last
      -- The Off_List position cannot be used for Delete, Get, or Put
      --
      -- Time: O(1)

      -- Operations to obtain valid positions from valid positions:
      function Next (Pos : Position) return Position; -- raise Invalid_Position
      function Prev (Pos : Position) return Position; -- raise Invalid_Position
      -- Next and Prev raise Invalid_Position if Pos is invalid
      --
      -- Time: O(1)

      -- Operations to manipulate lists
      procedure Insert (Item : in Element; Before : in Position; New_Pos : out Position);
      -- Inserts Item before Before
      -- Returns the position of Item in the list in New_pos
      -- Raises Storage_Exhausted if no more storage is available for this list
      -- Raises Invalid_Position if Pos is invalid
      -- Nothing is changed if Storage_Exhausted or Invalid_Position are raised
      --
      -- Time: O(1)

      procedure Append (Item : in Element; After : in Position; New_Pos : out Position);
      -- Appends Item after After
      -- Returns the position of Item in the list in New_Pos
      -- Raises Storage_Exhausted if no more storage is available for this list
      -- Raises Invalid_Position if Pos is invalid
      -- Nothing is changed if Storage_Exhausted or Invalid_Position are raised
      --
      -- Time: O(1)

      procedure Delete (Pos : in out Position); -- raise Invalid_Position
      -- Deletes the item at Pos
      -- Pos is made invalid
      -- Raises Invalid_Position if Pos is invalid or is the Off_List position for the list
      -- Nothing is changed if Invalid_Position is raised
      --
      -- Time: O(1)

      function Get (Pos : Position) return Element; -- raise Invalid_Position
      -- Returns the item at Pos
      -- Raises Invalid_Position if Pos is invalid or the Off_List position
      --
      -- Time: O(1)

      procedure Put (Pos : in Position; Item : in Element); -- raise Invalid_Position
      -- Makes the Element stored at Pos be Item
      -- Raises Invalid_Position if Pos is invalid or is the Off_List position for Into
      -- Nothing is changed if Invalid_Position is raised
      --
      -- Time: O(1)

      function Is_Empty return Boolean;
      -- Returns True if this list is empty (Length = 0); returns False otherwise
      --
      -- Time: O(1)

      function Length return Natural; -- Returns a count of the number of items in this list
      --
      -- Time: O(N)

      procedure Iterate (Action : access procedure (Item : in out Element; Pos : in Position; Continue : out Boolean) );
      -- Calls Action with each Element in the list, & its Position, in turn from First to Last
      -- Returns immediately if Continue is set to False (remainder of the list is not processed)

      procedure Sort (Less_Than : access function (Left : Element; Right : Element) return Boolean);
      -- Sorts the list into ascending order as defined by Less_Than
      -- Requires one additional list node
      -- Raises Storage_Exhausted if no more storage is available for this extra node
      -- The list is unchanged if Storage_Exhausted is raised
      --
      -- Time: O(N log N)
   private -- Handle
      List : Implementation.Handle;
   end Handle;
private -- PragmARC.List_Unbounded
   type Position is new Implementation.Position;
end PragmARC.List_Unbounded;
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
