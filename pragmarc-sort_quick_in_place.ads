-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic quick sort
--
-- History:
-- 2013 Mar 01     J. Carter          V2.0--Package with sequential and parallel sorts
-- 2002 Oct 01     J. Carter          V1.1--Use mode out to allow scalars
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Sort_Quick_In_Place
   type Element  is limited private;
   type Index    is (<>);
   type Sort_Set is array (Index range <>) of Element;

   with procedure Assign (To : out Element; From : in Element) is <>;
   with function "<" (Left : Element; Right : Element) return Boolean is <>;
package PragmARC.Sort_Quick_In_Place is
   procedure Sort_Sequential (Set : in out Sort_Set);
   -- Sorts Set.
   --
   -- Time: O(N log N)

   procedure Sort_Parallel (Set : in out Sort_Set; Max_Tasks : in Natural := 1);
   -- Sorts Set using up to Max_Tasks additional tasks.
   --
   -- Time: O(N log N)
end PragmARC.Sort_Quick_In_Place;
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
