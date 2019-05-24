-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic insertion sort
--
-- History:
-- 2019 Apr 15     J. Carter          V1.1--Sequences indexed by integers
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Sort_Insertion
   type Element  is private;
   type Index    is range <>; -- Lower bound should be 1
   type Sort_Set is array (Index range <>) of Element;

   with function "<" (Left : Element; Right : Element) return Boolean is <>;
procedure PragmARC.Sort_Insertion (Set : in out Sort_Set);
pragma Pure (PragmARC.Sort_Insertion);
-- Input:  Set to sort
-- Output: Sorted set
--
-- Time: O(N ** 2)
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
