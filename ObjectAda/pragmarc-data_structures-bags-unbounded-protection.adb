-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Feb 15     J. Carter          V1.2--Make more Object.Operation friendly
-- 2016 Jun 01     J. Carter          V1.1--Changed comments for empty declarative parts
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
--------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate
-- 2001 May 01     J. Carter          V1.2--Improved time complexity of Empty
-- 2000 Dec 01     J. Carter          V1.1--Revised implementation of Iterate
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Bags.Unbounded.Protection is
   protected body Handle is
      procedure Add (Item : in Element) is
         -- Empty
      begin -- Add
         Bag.Add (Item => Item);
      end Add;

      procedure Clear is
         -- Empty
      begin -- Clear
         Bag.Clear;
      end Clear;

      procedure Delete (Item : in Element) is
         -- Empty
      begin -- Delete
         Bag.Delete (Item => Item);
      end Delete;

      function Empty return Boolean is
         (Bag.Empty);

      function Find (Key : in Element) return Find_Result is
         Result : Implementation.Find_Result;
      begin -- Find
         Result := Bag.Find (Key);

         if Result.Found then
            return (Found => True, Item => Result.Item);
         else
            return (Found => False);
         end if;
      end Find;

      procedure Iterate (Action : access procedure (Item : in out Element) ) is
         procedure Local is new Implementation.Iterate (Action => Action.all);
      begin -- Iterate
         Local (Over => Bag);
      end Iterate;

      function Size return Natural is
         (Bag.Size);

      procedure Update (Item : in Element) is
         -- Empty
      begin -- Update
         Bag.Update (Item => Item);
      end Update;
   end Handle;
end PragmARC.Data_Structures.Bags.Unbounded.Protection;
