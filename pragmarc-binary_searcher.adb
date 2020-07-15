-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2020 Jul 15     J. Carter          V1.2--Improve calculation of Mid
-- 2019 Apr 15     J. Carter          V1.1--Require integer Index
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with System;

package body PragmARC.Binary_Searcher is
   function Search (Item : Element; Within : List) return Result_Info is
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
