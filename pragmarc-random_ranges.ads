-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Returns a uniformly distributed integer in Min .. Max
-- Internally, it declares a type mod 2 ** 33, so this won't work with compilers that have
-- System.Max_Binary_Modulus < 2 ** 33
--
-- History:
-- 2016 Oct 01     J. Carter     V1.0--Initial Version
--
with Interfaces;

generic -- PragmARC.Random_Ranges
   type Generator (<>) is limited private;

   with function Random (G : Generator) return Interfaces.Unsigned_32;
function PragmARC.Random_Ranges (G : Generator; Min : Interfaces.Unsigned_32; Max : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
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
