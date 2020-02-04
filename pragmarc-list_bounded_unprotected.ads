-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- General purpose list for sequential use only
-- A list has elements in sequence; each element has a position in that sequence
-- Positions are used to manipulate a list
-- Each list has a maximum length
--
-- History:
-- 2020 Feb 15     J. Carter          V1.2--Make more Object.Operation friendly
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.1--Added Context to Iterate; protect list IDs; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.0--Initial release
--
with Ada.Finalization;

generic -- PragmARC.List_Bounded_Unprotected
   type Element is private;
package PragmARC.List_Bounded_Unprotected is
   pragma Preelaborate;

   type Handle (Max_Size : Positive) is tagged limited private;
   -- Initial value: empty

   type Position is private;
   -- A position is initially invalid until assigned a value by First,  Last, or Off_List
   -- Other positions accessible via Next and Prev

   Invalid_Position : exception; -- Raised if a position is invalid

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- Raises Too_Short if To.Max_Size < Length (From)
   -- Nothing is changed if Too_Short is raised
   -- Time: O(N)
   --
   -- Precondition: To.Max_Size <= Length (From)     raises Too_Short if violated

   procedure Clear (List : in out Handle);
   -- Makes List empty; all lists are initially empty
   -- Time: O(N)
   --
   -- Postcondition: Is_Empty (List)

   -- Operations to obtain valid positions for lists:
   function First (List : Handle) return Position; -- The position of the first item in List
   function Last (List : Handle) return Position; -- The position of the last item in List
   function Off_List (List : Handle) return Position;
   -- Time: O(1)
   --
   -- Off_List (List) is the valid Position for List that is returned by Prev (First (List), List)
   -- and by Next (Last (List), List)
   -- First and Last return Off_List (List) if Is_Empty (List);

   -- Operations to obtain valid positions from valid positions:
   function Next (List : Handle; Pos : Position) return Position; -- raise Invalid_Position
   function Prev (List : Handle; Pos : Position) return Position; -- raise Invalid_Position
   -- Next and Prev raise Invalid_Position if Pos is invalid
   -- Next (Last (L), L) = Prev (First (L), L) = Off_List (L)
   -- Next (Off_List (L), L) = First (L)
   -- Prev (Off_List (L), L) = Last (L)
   --
   -- Time: O(1)

   -- Operations to manipulate lists
   procedure Insert (Into : in out Handle; Item : in Element; Before : in Position; New_Pos : out Position);
   -- Inserts Item before Before
   -- Returns the position of Item in Into in New_pos
   -- Before => Off_List (Into) is the same as Append with After => Last (Into)
   -- Raises Full if Into is full
   -- Raises Invalid_Position if Pos is invalid
   -- Nothing is changed if Full or Invalid_Position are raised
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Full (Into)     raise Full if violated
   -- Postcondition: not Is_Empty (Into)

   procedure Append (Into : in out Handle; Item : in Element; After : in Position; New_Pos : out Position);
   -- Appends Item after After
   -- Returns the position of Item in Into in New_Pos
   -- After => Off_List (Into) is the same as Insert with Before => First (Into)
   -- Raises Full if Into is full
   -- Raises Invalid_Position if Pos is invalid
   -- Nothing is changed if Full or Invalid_Position are raised
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Full (Into)     raise Full if violated
   -- Postcondition: not Is_Empty (Into)

   procedure Delete (From : in out Handle; Pos : in out Position); -- raise Invalid_Position
   -- Deletes the item at Pos
   -- Pos is made invalid
   -- Raises Empty if From is empty
   -- Raises Invalid_Position if Pos is invalid or Off_List (From)
   -- Nothing is changed if Invalid_Position is raised
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Empty (From)     raise Empty if violated
   -- Postcondition: not Is_Full (From)

   function Get (From : Handle; Pos : Position) return Element; -- raise Invalid_Position
   -- Returns the item at Pos
   -- Raises Invalid_Position if Pos is invalid or Off_List (From)
   -- Raises Empty if From is empty
   --
   -- Time: O(1)

   procedure Put (Into : in out Handle; Pos : in Position; Item : in Element); -- raise Invalid_Position
   -- Makes the Element stored at Pos be Item
   -- Raises Invalid_Position if Pos is invalid or Off_List (Into)
   -- Nothing is changed if Invalid_Position is raised
   --
   -- Time: O(1)
   --
   -- Postcondition: Get (Into, Pos) = Item

   function Is_Empty (List : Handle) return Boolean;
   -- Returns True if List is empty [Length (List) = 0]; returns False otherwise
   --
   -- Time: O(1)

   function Is_Full (List : Handle) return Boolean;
   -- Returns True if List is full [Length (List) = List.Max_Size];
   -- returns False otherwise
   --
   -- Time: O(1)

   function Length (List : Handle) return Natural;
   -- Returns the number of items in List
   --
   -- Time: O(1)

   generic -- Iterate
      with procedure Action (Item : in out Element; Pos : in Position; Continue : out Boolean);
   procedure Iterate (Over : in out Handle);
   -- Calls Action with each Element in Over, & its Position, in turn
   -- Returns immediately if Continue is set to False (remainder of Over is not processed)
private -- PragmARC.List_Bounded_Unprotected
   type List_ID is mod 2 ** 32;

   Invalid_ID : constant List_ID := 0;

   Null_Position : constant Natural := 0;

   type Position is record
      ID  : List_ID := Invalid_ID;
      Pos : Natural := Null_Position;
   end record;

   type Node is record
      Prev  : Natural;
      ID    : List_ID := Invalid_ID;
      Value : Element;
      Next  : Natural;
   end record;

   type List_Storage is array (Positive range <>) of Node;

   type Handle (Max_Size : Positive) is new Ada.Finalization.Limited_Controlled with record
      ID      : List_ID := Invalid_ID;
      Head    : Natural;
      Tail    : Natural;
      Length  : Natural;
      Storage : List_Storage (1 .. Max_Size);
      Free    : Natural := 1;
   end record;

   procedure Initialize (Object : in out Handle);
end PragmARC.List_Bounded_Unprotected;
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
