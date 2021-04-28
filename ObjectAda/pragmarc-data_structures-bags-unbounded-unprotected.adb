-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.2--Adhere to coding standard
-- 2021 Jan 01     J. Carter          V2.2--Removed Assign
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Mar 15     J. Carter          V1.4--Make List_Unbounded_Unprotected's Next and Prev Object.Operation friendly
-- 2020 Feb 15     J. Carter          V1.3--Make more Object.Operation friendly
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.2--Added Assign
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Bags.Unbounded.Unprotected is
   procedure Clear (Bag : in out Handle) is
      -- Empty
   begin -- Clear
      Bag.List.Clear;
   end Clear;

   procedure Add (Into : in out Handle; Item : in Element) is
      -- Empty
   begin -- Add
      Into.List.Prepend (New_Item => Item);
   end Add;

   type Search_Result (Found : Boolean := False) is record
      case Found is
      when False =>
         null;
      when True =>
         Pos : Implementation.Cursor;
      end case;
   end record;

   function Find (Key : in Element; Bag : in Handle) return Search_Result;
   -- Internal Find function used by Delete, Update, and Find
   -- Result points to matching Node if Found

   function Find (Key : in Element; Bag : in Handle) return Search_Result is
      Pos : constant Implementation.Cursor := Bag.List.Find (Key);

      use type Implementation.Cursor;
   begin -- Find
      if Pos = Implementation.No_Element then
         return (Found => False);
      end if;

      return (Found => True, Pos => Pos);
   end Find;

   procedure Delete (From : in out Handle; Item : in Element) is
      Temp : Search_Result := Find (Item, From);
   begin -- Delete
      if Temp.Found then
         From.List.Delete (Position => Temp.Pos);
      end if;
   end Delete;

   procedure Update (Bag : in out Handle; Item : in Element) is
      Temp : constant Search_Result := Find (Item, Bag);
   begin -- Update
      if Temp.Found then
         Bag.List.Replace_Element (Position => Temp.Pos, New_Item => Item);
      end if;
   end Update;

   function Find (Bag : in Handle; Key : in Element) return Find_Result is
      Temp : constant Search_Result := Find (Key, Bag);
   begin -- Find
      if Temp.Found then
         return (Found => True, Item => Implementation.Element (Temp.Pos) );
      end if;

      return (Found => False);
   end Find;

   function Empty (Bag : in Handle) return Boolean is
      (Bag.List.Is_Empty);

   function Size (Bag : in Handle) return Natural is
      (Integer (Bag.List.Length) );

   procedure Iterate (Over : in out Handle) is
      procedure Action (Position : in Implementation.Cursor) is
         Item : Element := Implementation.Element (Position);
      begin -- Action
         Action (Item => Item);
      end Action;
   begin -- Iterate
      Over.List.Iterate (Process => Action'Access);
   end Iterate;
end PragmARC.Data_Structures.Bags.Unbounded.Unprotected;
