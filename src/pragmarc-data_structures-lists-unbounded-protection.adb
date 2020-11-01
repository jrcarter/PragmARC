-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Mar 15     J. Carter          V1.2--Make List_Unbounded_Unprotected's Next and Prev Object.Operation friendly
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.3--Improved instantiation of Sort
-- 2001 May 01     J. Carter          V1.2--Added Is_Empty
-- 2000 Dec 01     J. Carter          V1.1--Revised implementation of Iterate
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Lists.Unbounded.Protection is
   protected body Handle is
      procedure Clear is
         -- Empty
      begin -- Clear
         List.Clear;
      end Clear;

      function First return Position is
         (Position (List.First) );

      function Last return Position is
         (Position (List.Last) );

      function Next (Pos : Position) return Position is
         -- Empty
      begin -- Next
         return Position (Implementation.Next (Implementation.Cursor (Pos) ) );
      exception -- Next
      when others =>
         raise Invalid_Position;
      end Next;

      function Prev (Pos : Position) return Position is
         -- Empty
      begin -- Prev
         return Position (Implementation.Previous (Implementation.Cursor (Pos) ) );
      exception -- Prev
      when others =>
         raise Invalid_Position;
      end Prev;

      procedure Insert (Item : in Element; Before : in Position) is
         -- Empty
      begin -- Insert
         List.Insert (New_Item => Item, Before => Implementation.Cursor (Before) );
      exception -- Insert
      when Storage_Error =>
         raise Storage_Exhausted;
      when others =>
         raise Invalid_Position;
      end Insert;

      procedure Append (Item : in Element) is
         -- Empty
      begin -- Append
         List.Append (New_Item => Item);
      exception -- Append
      when Storage_Error =>
         raise Storage_Exhausted;
      when others =>
         raise Invalid_Position;
      end Append;

      procedure Delete (Pos : in out Position) is
         -- Empty
      begin -- Delete
         List.Delete (Position => Implementation.Cursor (Pos) );
      exception -- Delete
      when Others =>
         raise Invalid_Position;
      end Delete;

      function Get (Pos : Position) return Element is
         -- Empty
      begin -- Get
         return Implementation.Element (Implementation.Cursor (Pos) );
      exception -- Get
      when Others =>
         raise Invalid_Position;
      end Get;

      procedure Put (Pos : in Position; Item : in Element) is
         -- Empty
      begin -- Put
         List.Replace_Element (Position => Implementation.Cursor (Pos), New_Item => Item);
      exception -- Put
      when Others =>
         raise Invalid_Position;
      end Put;

      function Is_Empty return Boolean is
         (List.Is_Empty);

      function Length return Natural is
         (Integer (List.Length) );

      procedure Iterate (Action : access procedure (Item : in Element; Pos : in Position) ) is
         procedure Local_Action (Pos : in Implementation.Cursor) is
            -- Empty
         begin -- Local_Action
            Action (Item => Implementation.Element (Pos), Pos => Position (Pos) );
         end Local_Action;
      begin -- Iterate
         List.Iterate (Process => Local_Action'Access);
      end Iterate;

      procedure Sort (Less_Than : access function (Left : Element; Right : Element) return Boolean) is
         package Sorting is new Implementation.Generic_Sorting ("<" => Less_Than.all);
      begin -- Sort
         Sorting.Sort (Container => List);
      end Sort;
   end Handle;
end PragmARC.Data_Structures.Lists.Unbounded.Protection;
