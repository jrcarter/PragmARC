-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- A package to obtain real-valued random numbers from generators that generate Unsigned_32 values (such as KISS and Threefry)

-- History:
-- 2016 Oct 01     J. Carter     V1.1--Removed Random_Range and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2013 Nov 01     J. Carter     V1.0--Initial release

with Interfaces;

generic -- PragmARC.Real_Random_Values
   type Supplied_Real is digits <>;
   type Generator (<>) is limited private;

   with function Unsigned_Random (State : in Generator) return Interfaces.Unsigned_32;
package PragmARC.Real_Random_Values is
   subtype Real is Supplied_Real'Base;

   function Random (State : in Generator) return Real;
   -- Returns a uniformly distributed random value in 0.0 .. 1.0 - 2 ** (-32)
end PragmARC.Real_Random_Values;
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
