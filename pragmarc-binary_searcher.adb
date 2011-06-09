-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Binary_Searcher is
   function Search (Item : Element; Within : List) return Result_Info is
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
         exit Find when Low = High or Low = Index'Pred (High);

         Mid := Index'Val ( (Index'Pos (Low) + Index'Pos (High) ) / 2);

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