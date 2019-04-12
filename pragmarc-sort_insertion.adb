-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic insertion sort
--
-- History:
-- 2019 Apr 15     J. Carter          V1.2--Sequences indexed by integers
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
procedure PragmARC.Sort_Insertion (Set : in out Sort_Set) is
   function ">=" (Left : Element; Right : Element) return Boolean is
      -- Empty
   begin -- ">="
      return not (Left < Right);
   end ">=";
   pragma Inline (">=");

   Temp : Element;
   J    : Index;
begin -- PragmARC.Sort_Insertion
   if Set'Length <= 1 then -- Already sorted
      return;
   end if;

   Search : for I in Set'First + 1 .. Set'Last loop -- Invariant: Set (Set'First .. I - 1) is sorted
      Temp := Set (I);
      J := I;

      Insert : loop
         exit Insert when J <= Set'First or else Temp >= Set (J - 1);

         Set (J) := Set (J - 1);
         J := J - 1;
      end loop Insert;

      Set (J) := Temp;
   end loop Search;
end PragmARC.Sort_Insertion;
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
