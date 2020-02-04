-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2020 Feb 15     J. Carter          V1.3--Make more Object.Operation friendly
-- 2018 Aug 01     J. Carter          V1.2--Make Length O(1)
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
-----------------------------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.1--Added Context to Iterate; protected list IDs; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.0--Initial release
--
with System;
package body PragmARC.List_Bounded_Unprotected is
   Invalid_Pos : constant Position := (ID => Invalid_ID, Pos => Null_Position);

   procedure Assign (To : out Handle; From : in Handle) is
      From_Pos : Natural := From.Head;
      To_Pos   : Natural := To.Storage'First;
   begin -- Assign
      if To.ID = From.ID then -- These are the same list
         return;
      end if;

      if To.Max_Size < Length (From) then
         raise Too_Short;
      end if;

      Clear (List => To);

      Copy : loop
         exit Copy when From_Pos = Null_Position;

         To.Storage (To_Pos).Value := From.Storage (From_Pos).Value;
         To.Storage (To_Pos).Prev := To_Pos - 1;
         To.Storage (To_Pos).ID := To.ID;

         if To_Pos /= To.Storage'First then
            To.Storage (To_Pos - 1).Next := To_Pos;
         end if;

         To.Storage (To_Pos).Next := Null_Position;

         if To_Pos = To.Storage'First then
            To.Head := To_Pos;
         end if;

         To_Pos := To_Pos + 1;
         From_Pos := From.Storage (From_Pos).Next;
      end loop Copy;

      To.Tail := To_Pos - 1;
      To.Free := To_Pos;
      To.Length := From.Length;
   end Assign;

   protected ID_Supply is
      pragma Priority (System.Priority'Last);

      procedure Get (ID : out List_ID);
   private -- ID_Supply
      Last_ID : List_ID := Invalid_ID;
   end ID_Supply;

   procedure Clear (List : in out Handle) is
      -- Empty
   begin -- Clear
      List.Head := Null_Position;
      List.Tail := Null_Position;
      List.Free := List.Storage'First;
      List.Length := 0;

      if List.ID = Invalid_ID then
         ID_Supply.Get (ID => List.ID);
      end if;

      Clear_Storage : for I in List.Storage'range loop
         List.Storage (I).Prev := Null_Position;
         List.Storage (I).Next := I + 1;
         List.Storage (I).ID := Invalid_ID;
      end loop Clear_Storage;

      List.Storage (List.Storage'Last).Next := Null_Position;
   end Clear;

   function First (List : Handle) return Position is
      -- Empty
   begin -- First
      return (ID => List.ID, Pos => List.Head);
   end First;

   function Last (List : Handle) return Position is
      -- Empty
   begin -- Last
      return (ID => List.ID, Pos => List.Tail);
   end Last;

   function Off_List (List : Handle) return Position is
      -- Empty
   begin -- Off_List
      return (ID => List.ID, Pos => Null_Position);
   end Off_List;

   function Next (List : Handle; Pos : Position) return Position is
      -- Empty
   begin -- Next
      if Pos.ID /= List.ID then
         raise Invalid_Position;
      end if;

      if Pos.Pos = Null_Position then
         return (ID => List.ID, Pos => List.Head);
      else
         return (ID => List.ID, Pos => List.Storage (Pos.Pos).Next);
      end if;
   end Next;

   function Prev (List : Handle; Pos : Position) return Position is
      -- Empty
   begin -- Prev
      if Pos.ID /= List.ID then
         raise Invalid_Position;
      end if;

      if Pos.Pos = Null_Position then
         return (ID => List.ID, Pos => List.Tail);
      else
         return (ID => List.ID, Pos => List.Storage (Pos.Pos).Prev);
      end if;
   end Prev;

   procedure Insert (Into : in out Handle; Item : in Element; Before : in Position; New_Pos : out Position) is
      Prev : Natural;
   begin -- Insert
      if Into.Free = Null_Position then
         raise Full;
      end if;

      -- Special case: Into is empty; Before must be Off_List

      if Into.Head = Null_Position then
         if Before /= (ID => Into.ID, Pos => Null_Position) then
            raise Invalid_Position;
         end if;

         New_Pos := (ID => Into.ID, Pos => Into.Free);
         Into.Free := Into.Storage (Into.Free).Next;
         Into.Storage (New_Pos.Pos).Value := Item;
         Into.Head := New_Pos.Pos;
         Into.Tail := New_Pos.Pos;
         Into.Storage (New_Pos.Pos).Prev := Null_Position;
         Into.Storage (New_Pos.Pos).Next := Null_Position;
         Into.Storage (New_Pos.Pos).ID := Into.ID;
         Into.Length := Into.Length + 1;

         return;
      end if;

      if Before = (ID => Into.ID, Pos => Null_Position) then -- Same as Append after Last
         Append (Into => Into, Item => Item, After => (ID => Into.ID, Pos => Into.Tail), New_Pos => New_Pos);

         return;
      end if;

      if Before.ID /= Into.ID or Before.Pos not in Into.Storage'range then
         raise Invalid_Position;
      end if;

      New_Pos := (ID => Into.ID, Pos => Into.Free);
      Into.Free := Into.Storage (Into.Free).Next;

      Into.Storage (New_Pos.Pos).Value := Item;

      Into.Storage (New_Pos.Pos).Next := Before.Pos;
      Into.Storage (New_Pos.Pos).Prev := Into.Storage (Before.Pos).Prev;
      Into.Storage (Before.Pos).Prev := New_Pos.Pos;
      Into.Storage (New_Pos.Pos).ID := Into.ID;
      Into.Length := Into.Length + 1;

      Prev := Into.Storage (New_Pos.Pos).Prev;

      if Prev = Null_Position then
         Into.Head := New_Pos.Pos;
      else
         Into.Storage (Prev).Next := New_Pos.Pos;
      end if;

      if Into.Storage (New_Pos.Pos).Next = Null_Position then
         Into.Tail := New_Pos.Pos;
      end if;
   end Insert;

   procedure Append (Into : in out Handle; Item : in Element; After : in Position; New_Pos : out Position) is
      Next : Natural;
   begin -- Append
      if Into.Free = Null_Position then
         raise Full;
      end if;

      -- Special case: Into is empty; same as Insert

      if Into.Head = Null_Position then
         Insert (Into => Into, Item => Item, Before => After, New_Pos => New_Pos);

         return;
      end if;

      if After = (ID => Into.ID, Pos => Null_Position) then -- Same as Insert before First
         Insert (Into => Into, Item => Item, Before => (ID => Into.ID, Pos => Into.Head), New_Pos => New_Pos);

         return;
      end if;

      if After.ID /= Into.ID or After.Pos not in Into.Storage'range then
         raise Invalid_Position;
      end if;

      New_Pos := (ID => Into.ID, Pos => Into.Free);
      Into.Free := Into.Storage (Into.Free).Next;

      Into.Storage (New_Pos.Pos).Value := Item;

      Into.Storage (New_Pos.Pos).Prev := After.Pos;
      Into.Storage (New_Pos.Pos).Next := Into.Storage (After.Pos).Next;
      Into.Storage (After.Pos).Next := New_Pos.Pos;
      Into.Storage (New_Pos.Pos).ID := Into.ID;
      Into.Length := Into.Length + 1;

      Next := Into.Storage (New_Pos.Pos).Next;

      if Next = Null_Position then
         Into.Tail := New_Pos.Pos;
      else
         Into.Storage (Next).Prev := New_Pos.Pos;
      end if;

      if Into.Storage (New_Pos.Pos).Prev = Null_Position then
         Into.Head := New_Pos.Pos;
      end if;
   end Append;

   procedure Delete (From : in out Handle; Pos : in out Position) is
      Prev : Natural;
      Next : Natural;
   begin -- Delete
      if Pos.ID /= From.ID or (Pos.Pos not in From.Storage'range or else From.Storage (Pos.Pos).ID /= From.ID) then
         raise Invalid_Position;
      end if;

      Prev := From.Storage (Pos.Pos).Prev;
      Next := From.Storage (Pos.Pos).Next;

      if From.Head = Pos.Pos then
         From.Head := Next;
      end if;

      if From.Head = Null_Position then -- List is now empty
         Clear (List => From);
      else
         if From.Tail = Pos.Pos then
            From.Tail := Prev;
         end if;

         if Prev /= Null_Position then
            From.Storage (Prev).Next := Next;
         end if;

         if Next /= Null_Position then
            From.Storage (Next).Prev := Prev;
         end if;

         From.Storage (Pos.Pos).Prev := Null_Position;
         From.Storage (Pos.Pos).Next := From.Free;
         From.Storage (Pos.Pos).ID := Invalid_ID;
         From.Free := Pos.Pos;
         From.Length := From.Length - 1;
      end if;

      Pos := Invalid_Pos;
   end Delete;

   function Get (From : Handle; Pos : Position) return Element is
      -- Empty
   begin -- Get
      if Pos.ID /= From.ID or (Pos.Pos not in From.Storage'range or else From.Storage (Pos.Pos).ID /= From.ID) then
         raise Invalid_Position;
      end if;

      return From.Storage (Pos.Pos).Value;
   end Get;

   procedure Put (Into : in out Handle; Pos : in Position; Item : in Element) is
      -- Empty
   begin -- Put
      if Pos.ID /= Into.ID or (Pos.Pos not in Into.Storage'range or else Into.Storage (Pos.Pos).ID /= Into.ID) then
         raise Invalid_Position;
      end if;

      Into.Storage (Pos.Pos).Value := Item;
   end Put;

   function Is_Empty (List : Handle) return Boolean is
      -- Empty
   begin -- Is_Empty
      return List.Head = Null_Position;
   end Is_Empty;

   function Is_Full (List : Handle) return Boolean is
      -- Empty
   begin -- Is_Full
      return List.Free = Null_Position;
   end Is_Full;

   function Length (List : Handle) return Natural is
      -- Empty
   begin -- Length
      return List.Length;
   end Length;

   procedure Iterate (Over : in out Handle) is
      Pos      : Natural := Over.Head;
      Continue : Boolean;
   begin
      All_Nodes : loop
         exit All_Nodes when Pos = Null_Position;

         Action (Item => Over.Storage (Pos).Value, Pos => (ID => Over.ID, Pos => Pos), Continue => Continue);

         exit All_Nodes when not Continue;

         Pos := Over.Storage (Pos).Next;
      end loop All_Nodes;
   end Iterate;

   procedure Initialize (Object : in out Handle) renames Clear;

   protected body ID_Supply is
      procedure Get (ID : out List_ID) is
         -- Empty
      begin -- Get
         Last_ID := Last_ID + 1;

         if Last_ID = Invalid_ID then
            Last_ID := Invalid_ID + 1;
         end if;

         ID := Last_ID;
      end Get;
   end ID_Supply;
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
