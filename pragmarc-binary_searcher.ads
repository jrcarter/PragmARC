-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Binary search of an ordered list
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Jun 01     J. Carter          V1.1--Require Index'First = 1
-- 2019 Apr 15     J. Carter          V1.0--Require integer Index
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Three_Way;

generic -- PragmARC.Binary_Searcher
   type Element is limited private;
   type Index is range <>; -- Lower bound of 1; see pragma Assert below
   type List is array (Index range <>) of Element;

   with function Compare (Left : Element; Right : Element) return Three_Way.Relation_Id is <>;
package PragmARC.Binary_Searcher with Pure is
   pragma Assert (Index'First = 1);

   type Outside_Id is (First, Last);

   type Unfound_Info (Between : Boolean := False) is record
      case Between is
      when False =>
         Location : Outside_Id := First;
      when True =>
         Close : Index := Index'First;
      end case;
   end record;

   type Result_Info (Found : Boolean := False) is record
      case Found is
      when False =>
         Unfound : Unfound_Info;
      when True =>
         Location : Index := Index'First;
      end case;
   end record;

   function Search (Item : in Element; Within : in List) return Result_Info with
      Pre => (Within'Length > 0 and then
              (for all I in Within'First .. Within'Last - 1 =>
                  Compare (Within (I), Within (I + 1) ) in Three_Way.Less .. Three_Way.Equal) ) or else
             raise Constraint_Error;
   -- Searches for Item in the ordered List Within using a binary search, O(log N).
   -- If there is a component I of Within such that Compare (Within (I), Item) = Equal,
   -- returns (Found => True, Location => I). If there is more than one such component, the function is
   -- determinate (gives the same result for the same inputs), but the value of Location is arbitrary.
   -- If there is no such component of Within, the result has Found = False and Unfound set to indicate
   -- where in the List Item should be inserted to keep the List ordered.
   -- If Compare (Within (Within'First), Item) in Less  .. Equal and
   --    Compare (Within (Within'Last),  Item) in Equal .. Greater,
   -- then Item belongs in the List. In this case, Unfound.Between = True and Unfound.Close is set such
   -- that Compare (Within (Unfound.Close),             Item) = Less and
   --      Compare (Within (Index'Succ (Unfound.Close), Item) = Greater.
   -- If Item does not belong in the List, Unfound.Between = False and Unfound.Location indicates where
   -- Item belongs outside the List. Unfound.Location = First indicates that
   -- Compare (Within (Within'First), Item) = Greater, so Item belongs First in the List.
   -- Unfound.Location = Last indicates that Compare (Within (Within'Last), Item) = Less, so Item belongs
   -- Last in the List.
end PragmARC.Binary_Searcher;
