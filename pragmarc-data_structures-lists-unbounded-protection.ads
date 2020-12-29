-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- General purpose list for general use
-- A list has elements in sequence; each element has a position in that sequence
-- Positions are used to manipulate a list
-- This is based on Ada.Containers.Doubly_Linked_Lists and all the restrictions of that apply
--
-- History:
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.2--Added Ceiling_Priority to Handle
-- 2001 May 01     J. Carter          V1.1--Added Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Doubly_Linked_Lists;

with System;

generic -- PragmARC.Data_Structures.Lists.Unbounded.Protection
   type Element is private;
package PragmARC.Data_Structures.Lists.Unbounded.Protection with Preelaborate is
   package Implementation is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Element);

   type Position is private;
   -- A position is initially invalid until assigned a value by First, Last, or Off_List
   -- Other positions accessible via Next and Prev

   No_Element : constant Position;

   Invalid_Position : exception; -- Raised if a position is invalid or if the Off_List position is used for Delete, Get, or Put

   protected type Handle (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is -- Initial value: Empty
      pragma Priority (Ceiling_Priority);

      procedure Clear with
         Post => Is_Empty;

      -- Operations to obtain valid positions for lists:
      function First return Position; -- The position of the first item in the list
      function Last  return Position; -- The position of the last  item in the list
      -- List.Next (List.Last) and List.Prev (List.First) return No_Element
      --
      -- If List is empty, No_Element = List.First = List.Last

      -- Operations to obtain valid positions from valid positions:
      function Next (Pos : Position) return Position; -- raise Invalid_Position
      function Prev (Pos : Position) return Position; -- raise Invalid_Position
      -- Next and Prev raise Invalid_Position if Pos is invalid

      -- Operations to manipulate lists
      procedure Insert (Item : in Element; Before : in Position) with
         Post => not Is_Empty;
      -- Inserts Item before Before
      -- Returns the position of Item in the list in New_pos
      -- Raises Storage_Exhausted if no more storage is available for this list
      -- Raises Invalid_Position if Pos is invalid
      -- Nothing is changed if Storage_Exhausted or Invalid_Position are raised

      procedure Append (Item : in Element) with
         Post => not Is_Empty;
      -- Appends Item to the end of the list
      -- Raises Storage_Exhausted if no more storage is available for this list
      -- Raises Invalid_Position if Pos is invalid
      -- Nothing is changed if Storage_Exhausted or Invalid_Position are raised

      procedure Delete (Pos : in out Position); -- raise Invalid_Position
      -- Deletes the item at Pos
      -- Pos is made invalid
      -- Raises Invalid_Position if Pos is invalid or is No_Element
      -- Nothing is changed if Invalid_Position is raised

      function Get (Pos : Position) return Element; -- raise Invalid_Position
      -- Returns the item at Pos
      -- Raises Invalid_Position if Pos is invalid or the Off_List position

      procedure Put (Pos : in Position; Item : in Element); -- raise Invalid_Position
      -- Makes the Element stored at Pos be Item
      -- Raises Invalid_Position if Pos is invalid or is No_Element
      -- Nothing is changed if Invalid_Position is raised

      function Is_Empty return Boolean;
      -- Returns True if this list is empty (Length = 0); returns False otherwise

      function Length return Natural; -- Returns the number of items in this list

      procedure Iterate (Action : access procedure (Item : in Element; Pos : in Position) );
      -- Calls Action with each Element in the list, & its Position, in turn from First to Last

      procedure Sort (Less_Than : access function (Left : Element; Right : Element) return Boolean);
      -- Sorts the list into ascending order as defined by Less_Than using the sort from Ada.Containers.Doubly_Linked_Lists
   private -- Handle
      List : Implementation.List;
   end Handle;
private -- PragmARC.Data_Structures.Lists.Unbounded.Protection
   type Position is new Implementation.Cursor;

   No_Element : constant Position := Position (Implementation.No_Element);
end PragmARC.Data_Structures.Lists.Unbounded.Protection;
