-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Binary search of an ordered list
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Three_Way;
generic -- PragmARC.Binary_Searcher
   type Element is limited private;
   type Index is (<>);
   type List is array (Index range <>) of Element;

   with function Compare (Left : Element; Right : Element) return Three_Way.Relation_Id is <>;
package PragmARC.Binary_Searcher is
   pragma Pure;

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

   function Search (Item : Element; Within : List) return Result_Info;
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
   --
   -- Preconditions : Within'Length > 0                           raise Constraint_Error if violated
   --                 for I in Within'First .. Index'Pred (Within'Last)
   --                    Compare (Within (I), Within (Index'Succ (I) ) ) in Less .. Equal
   --                    (Within is ordered)                      Result is undefined if violated
end PragmARC.Binary_Searcher;
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