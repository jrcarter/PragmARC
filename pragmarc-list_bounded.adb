-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2020 Feb 15     J. Carter          V1.2--Make more Object.Operation friendly
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.1--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.List_Bounded is
   protected body Handle is
      procedure Clear is
         -- Empty
      begin -- Clear
         List.Clear;
      end Clear;

      function First return Position is
         -- Empty
      begin -- First
         return Position (List.First);
      end First;

      function Last return Position is
         -- Empty
      begin -- Last
         return Position (List.Last);
      end Last;

      function Off_List return Position is
         -- Empty
      begin -- Off_List
         return Position (List.Off_List);
      end Off_List;

      function Next (Pos : Position) return Position is
         -- Empty
      begin -- Next
         return Position (List.Next (Implementation.Position (Pos) ) );
      exception -- Next
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Next;

      function Prev (Pos : Position) return Position is
         -- Empty
      begin -- Prev
         return Position (List.Prev (Implementation.Position (Pos) ) );
      exception -- Prev
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Prev;

      procedure Insert (Item : in Element; Before : in Position; New_Pos : out Position) is
         Result : Implementation.Position;
      begin -- Insert
         List.Insert (Item => Item, Before => Implementation.Position (Before), New_Pos => Result);
         New_Pos := Position (Result);
      exception -- Insert
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Insert;

      procedure Append (Item : in Element; After : in Position; New_Pos : out Position) is
         Result : Implementation.Position;
      begin -- Append
         List.Append (Item => Item, After => Implementation.Position (After), New_Pos => Result);
         New_Pos := Position (Result);
      exception -- Append
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Append;

      procedure Delete (Pos : in out Position) is
         -- Empty
      begin -- Delete
         List.Delete (Pos => Implementation.Position (Pos) );
      exception -- Delete
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Delete;

      function Get (Pos : Position) return Element is
         -- Empty
      begin -- Get
         return List.Get (Implementation.Position (Pos) );
      exception -- Get
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Get;

      procedure Put (Pos : in Position; Item : in Element) is
         -- Empty
      begin -- Put
         List.Put (Pos => Implementation.Position (Pos), Item => Item);
      exception -- Put
      when Implementation.Invalid_Position =>
         raise Invalid_Position;
      end Put;

      function Is_Empty return Boolean is
         -- Empty
      begin -- Is_Empty
         return List.Is_Empty;
      end Is_Empty;

      function Is_Full return Boolean is
         -- Empty
      begin -- Is_Full
         return List.Is_Full;
      end Is_Full;

      function Length return Natural is
         -- Empty
      begin -- Length
         return List.Length;
      end Length;

      procedure Iterate (Action : access procedure (Item : in out Element; Pos : In Position; Continue : out Boolean) ) is
         procedure Local_Action (Item : in out Element; Pos : In Implementation.Position; Continue : out Boolean) is
            -- Empty
         begin -- Local_Action
            Action (Item => Item, Pos => Position (Pos), Continue => Continue);
         end Local_Action;
         pragma Inline (Local_Action);

         procedure Local is new Implementation.Iterate (Action => Local_Action);
      begin -- Iterate
         Local (Over => List);
      end Iterate;
   end Handle;
end PragmARC.List_Bounded;
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
