-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2020 Mar 15     J. Carter          V1.4--Make Next and Prev Object.Operation friendly
-- 2018 Aug 01     J. Carter          V1.3--Make Length O(1)
-- 2016 Jun 01     J. Carter          V1.2--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.1--Added exception handler to Finalize
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------
-- 2011 Jul 01     J. Carter          V1.4--Finalize may be called multiple times
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.2--Added Assign
-- 2001 May 01     J. Carter          V1.1--Added Is_Empty; eliminated some duplicated code
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Unchecked_Deallocation;
package body PragmARC.List_Unbounded_Unprotected is
   type Node (Has_Data : Boolean) is record
      Prev    : Link;
      Next    : Link;
      List_Id : Link;

      case Has_Data is
      when False =>
         null;
      when True =>
         Value : Element;
      end case;
   end record;

   procedure Dispose is new Unchecked_Deallocation (Object => Node, Name => Link);

   function Initialize return Link is
      Result : Link;
   begin -- Initialize
      Result := new Node (Has_Data => False);
      Result.Prev := Result; -- Link it to itself to become an Off_List node of an empty list
      Result.Next := Result;
      Result.List_Id := Result;

      return Result;
   exception -- Initialize
   when Storage_Error =>
      raise Storage_Exhausted;
   end Initialize;

   procedure Assign (To : out Handle; From : in Handle) is
      From_Pos : Position := First (From);
      New_Pos  : Position;
   begin -- Assign
      if To.Off_List = From.Off_List then -- These are the same list
         return;
      end if;

      Clear (List => To);

      Copy : loop
         exit Copy when From_Pos = Off_List (From);

         Append (Into => To, Item => Get (From, From_Pos), After => Last (To), New_Pos => New_Pos);
         From_Pos := From.Next (From_Pos);
      end loop Copy;
   end Assign;

   procedure Clear (List : in out Handle) is
      Temp : Link := List.Off_List.Next; -- Point to first node
   begin -- Clear
      if Temp = List.Off_List then -- List is already empty
         return;
      end if;

      Invalidate : loop -- Make all nodes invalid
         exit Invalidate when Temp = List.Off_List;

         Temp.List_Id := null;
         Temp := Temp.Next;
      end loop Invalidate;

      Free_All : loop
         Temp := List.Off_List.Next;

         exit Free_All when Temp = List.Off_List;
         -- assert: Temp /= List.Off_List;

         List.Off_List.Next := Temp.Next;
         Dispose (X => Temp);
      end loop Free_All;

      List.Off_List.Prev := List.Off_List; -- Link Off_List node to itself
      List.Off_List.Next := List.Off_List;
      List.Length := 0;
   end Clear;

   function First (List : Handle) return Position is
      -- Empty
   begin -- First
      return Position'(List_Id => List.Off_List, Ptr => List.Off_List.Next);
   end First;

   function Last (List : Handle) return Position is
      -- Empty
   begin -- Last
      return Position'(List_Id => List.Off_List, Ptr => List.Off_List.Prev);
   end Last;

   function Off_List (List : Handle) return Position is
      -- Empty
   begin -- Off_List
      return Position'(List_Id => List.Off_List, Ptr => List.Off_List);
   end Off_List;

   procedure Check_Valid (List : in Handle; Pos : in Position) is -- Help procedure to check that Pos is valid for List
      -- Empty
   begin -- Check_Valid
      if Pos.List_Id /= List.Off_List or else Pos.Ptr = null or else Pos.Ptr.List_Id /= List.Off_List then
         raise Invalid_Position;
      end if;
   end Check_Valid;
   pragma Inline (Check_Valid);

   function Next (List : Handle; Pos : Position) return Position is
      -- Empty
   begin -- Next
      Check_Valid (List => List, Pos => Pos);

      return Position'(List_Id => Pos.List_Id, Ptr => Pos.Ptr.Next);
   end Next;

   function Prev (List : Handle; Pos : Position) return Position is
      -- Empty
   begin -- Prev
      Check_Valid (List => List, Pos => Pos);

      return Position'(List_Id => Pos.List_Id, Ptr => Pos.Ptr.Prev);
   end Prev;

   function Get return Link is -- Help function to get a new node and convert Storage_Error to Storage_Exhausted
      -- Empty
   begin -- Get
      return new Node (Has_Data => True);
   exception -- Get
   when Storage_Error =>
      raise Storage_Exhausted;
   end Get;
   pragma Inline (Get);

   procedure Insert (Into : in out Handle; Item : in Element; Before : in Position; New_Pos : out Position) is
      Temp : Link;
   begin -- Insert
      Check_Valid (List => Into, Pos => Before);

      Temp := Get;
      Temp.Value := Item;
      Temp.List_Id := Into.Off_List;
      Temp.Next := Before.Ptr; -- Link node into list
      Temp.Prev := Before.Ptr.Prev;
      Temp.Prev.Next := Temp;
      Temp.Next.Prev := Temp;
      Into.Length := Into.Length + 1;
      New_Pos := Position'(List_Id => Before.List_Id, Ptr => Temp);
   end Insert;

   procedure Append (Into : in out Handle; Item : in Element; After : in Position; New_Pos : out Position) is
      Temp : Link;
   begin -- Append
      Check_Valid (List => Into, Pos => After);

      Temp := Get;
      Temp.Value := Item;
      Temp.List_Id := Into.Off_List;
      Temp.Next := After.Ptr.Next; -- Link node into list
      Temp.Prev := After.Ptr;
      Temp.Prev.Next := Temp;
      Temp.Next.Prev := Temp;
      Into.Length := Into.Length + 1;
      New_Pos := Position'(List_Id => After.List_Id, Ptr => Temp);
   end Append;

   procedure Check_Valid_And_Not_Off (List : in Handle; Pos : in Position) is
      -- Help procedure to check that Pos is Valid for List and not the Off_List position for List
   begin -- Check_Valid_And_Not_Off
      if Pos.List_Id /= List.Off_List or else -- Pos not for List
         Pos.Ptr = null               or else -- Pos invalid
         Pos.Ptr = List.Off_List      or else -- Pos is Off_List for List
         Pos.Ptr.List_Id /= List.Off_List     -- Pos points to invalid node
      then
         raise Invalid_Position;
      end if;
   end Check_Valid_And_Not_Off;
   pragma Inline (Check_Valid_And_Not_Off);

   procedure Delete (From : in out Handle; Pos : in out Position) is
      -- Empty
   begin -- Delete
      Check_Valid_And_Not_Off (List => From, Pos => Pos);

      Pos.Ptr.Next.Prev := Pos.Ptr.Prev; -- Unlink node from list
      Pos.Ptr.Prev.Next := Pos.Ptr.Next;

      Pos.Ptr.List_Id := null; -- Make node invalid

      Dispose (X => Pos.Ptr);
      From.Length := From.Length - 1;

      Pos := Position'(List_Id => null, Ptr => null); -- Make Pos invalid
   end Delete;

   function Get (From : Handle; Pos : Position) return Element is
      -- Empty
   begin -- Get
      Check_Valid_And_Not_Off (List => From, Pos => Pos);

      return Pos.Ptr.Value;
   end Get;

   procedure Put (Into : in out Handle; Pos : in Position; Item : in Element) is
      -- Empty
   begin -- Put
      Check_Valid_And_Not_Off (List => Into, Pos => Pos);

      Pos.Ptr.Value := Item;
   end Put;

   function Is_Empty (List : Handle) return Boolean is
      -- Empty
   begin -- Is_Empty
      return List.Off_List.Next = List.Off_List;
   end Is_Empty;

   function Length (List : Handle) return Natural is
      -- Empty
   begin -- Length
      return List.Length;
   end Length;

   procedure Finalize (Object : in out Handle) is
      -- Empty
   begin -- Finalize
      if Object.Off_List /= null then
         Clear (List => Object);
         Dispose (X => Object.Off_List);
      end if;
   exception -- Finalize
   when others =>
      null;
   end Finalize;

   procedure Iterate (Over : in out Handle) is
      Pos      : Position := First (Over);
      Continue : Boolean;
      Item     : Element;
   begin -- Iterate
      All_Nodes : loop
         exit All_Nodes when Pos = Off_List (Over);

         Item := Get (Over, Pos);
         Action (Item => Item, Pos => Pos, Continue => Continue);
         Put (Into => Over, Pos => Pos, Item => Item);

         exit All_Nodes when not Continue;

         Pos := Over.Next (Pos);
      end loop All_Nodes;
   end Iterate;

   -- Merge sort, an O (N log N) time sort
   -- Normally, merge sort is O (N) in space, but when applied to a linked list, it becomes O (1) in space
   procedure Sort (List : in out Handle) is
      List_Length : constant Natural := Length (List);

      Head          : Link := First (List).Ptr;
      New_Head      : Link;
      Temp          : Link := Last (List).Ptr;
      Subset_Length : Positive := 1;
      Left          : Link;
      Right         : Link;
      Rest          : Link;

      procedure Unlink (Ptr : in Link) is -- Unlink merged node from its subset
         -- Empty
      begin -- Unlink
         if Ptr.Next /= null then
            Ptr.Next.Prev := null;
            Ptr.Next := null;
         end if;
      end Unlink;
      pragma Inline (Unlink);

      procedure Empty (Ptr : in out Link; Temp : in out Link) is
         -- Add any remaining nodes in subset pointed to by Ptr to sorted, merged subset with last node pointed to by Temp
      begin -- Empty
         Empty_Subset : loop -- Add any remaining nodes in subset
            exit Empty_Subset when Ptr = null;

            Temp.Next := Ptr;
            Ptr := Ptr.Next;
            Temp.Next.Prev := Temp;
            Temp := Temp.Next;

            Unlink (Ptr => Temp);
         end loop Empty_Subset;
      end Empty;
   begin -- Sort
      New_Head := new Node (Has_Data => False);
      Temp.Next := null;

      All_Lengths : loop
         exit All_Lengths when Subset_Length >= List_Length;

         Temp := New_Head;
         Left := Head;

         All_Subsets : loop
            exit All_Subsets when Left = null;

            Right := Left; -- Find end of left subset and beginning of right subset
            Find_Right : for I in 1 .. Subset_Length loop
               exit Find_Right when Right = null;

               Right := Right.Next;
            end loop Find_Right;

            if Right /= null then -- Unlink left subset from right subset
               Right.Prev.Next := null;
            end if;

            Rest := Right; -- Find end of right subset and beginning of rest of list
            Find_Rest : for I in 1 .. Subset_Length loop
               exit Find_Rest when Rest = null;

               Rest := Rest.Next;
            end loop Find_Rest;

            if Rest /= null then -- Unlink right subset from rest of list
               Rest.Prev.Next := null;
            end if;

            None_Empty : loop -- Merge the two subsets, Left & Right, until one becomes empty
               exit None_Empty when Left = null or Right = null;

               if Left.Value < Right.Value then -- Merge one value
                  Temp.Next := Left;
                  Left := Left.Next;
               else
                  Temp.Next := Right;
                  Right := Right.Next;
               end if;

               Temp.Next.Prev := Temp;
               Temp := Temp.Next;

               Unlink (Ptr => Temp);
            end loop None_Empty;
            -- assert: Left = null or Right = null

            Empty (Ptr => Left,  Temp => Temp); -- Add any remaining nodes in left  subset
            Empty (Ptr => Right, Temp => Temp); -- Add any remaining nodes in right subset

            Left := Rest; -- Repeat for rest of list
         end loop All_Subsets;

         Subset_Length := 2 * Subset_Length;
         Head := New_Head.Next; -- Repeat with longer subsets
      end loop All_Lengths;
      -- Now the list is sorted, but connected to New_Head
      -- Head points to the first item in the sorted list, Temp to the last item

      -- Link sorted list back into list structure
      List.Off_List.Next := Head;
      List.Off_List.Prev := Temp;
      Temp.Next := List.Off_List;
      Head.Prev := List.Off_List;

      Dispose (X => New_Head);
   exception -- Sort
   when Storage_Error =>
      raise Storage_Exhausted;
   end Sort;
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
