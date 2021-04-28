-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Jul 15     J. Carter          V1.2--Improve calculation of Mid
-- 2019 Apr 15     J. Carter          V1.1--Require integer Index
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with System;

package body PragmARC.Binary_Searcher is
   function Search (Item : in Element; Within : in List) return Result_Info is
      type Big is range System.Min_Int .. System.Max_Int;

      Low  : Index := Within'First;
      High : Index := Within'Last;
      Mid  : Index;

      function Less    return Three_Way.Relation_Id renames Three_Way.Less;
      function Equal   return Three_Way.Relation_Id renames Three_Way.Equal;
      function Greater return Three_Way.Relation_Id renames Three_Way.Greater;

      use type Three_Way.Relation_Id;
   begin -- Search
      if Compare (Within (Within'First), Item) = Greater then
         return (Found => False, Unfound => (Between => False, Location => First) );
      end if;

      if Compare (Within (Within'Last), Item) = Less then
         return (Found => False, Unfound => (Between => False, Location => Last) );
      end if;

      Find : loop
         exit Find when Low in High - 1 .. High;

         if Big'Last - Big (Low) < Big (High) then
            Mid := Index (Big (Low) + Big (High - Low) / 2);
         else
            Mid := Index ( (Big (Low) + Big (High) ) / 2);
         end if;

         case Compare (Item, Within (Mid) ) is
         when Three_Way.Less =>
            High := Mid;
         when Three_Way.Equal =>
            return (Found => True, Location => Mid);
         when Three_Way.Greater =>
            Low := Mid;
         end case;
      end loop Find;

      if Compare (Within (Low), Item) = Equal then
         return (Found => True, Location => Low);
      end if;

      if Low < High and then Compare (Within (High), Item) = Equal then
         return (Found => True, Location => High);
      end if;

      return (Found => False, Unfound => (Between => True, Close => Low) );
   end Search;
end PragmARC.Binary_Searcher;
