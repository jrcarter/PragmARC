-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Creates a random values in a range from a random value R such that 0 <= R < 1
--
-- History:
-- 2016 Oct 01     J. Carter          V1.0--Initial version
--
generic -- PragmARC.Real_Random_Ranges
   type Supplied_Real is digits <>;
package PragmARC.Real_Random_Ranges is
   subtype Real is Supplied_Real'Base;
   subtype Uniform is Real range 0.0 .. Real'Adjacent (1.0, 0.0);

   function Random_Range (R : Uniform; Min : Real; Max : Real) return Real;
   -- Converts R into a value in Min .. Max

   function Random_Int (R : Uniform; Min : Integer; Max : Integer) return Integer;
   -- Converts R into a value in Min .. Max

   type Normal_List is array (1 .. 12) of Uniform;

   function Normal (List : Normal_List; Mean : Real; Sigma : Real) return Real;
   -- Uses the random values in List to approximate a normally distributed random number with the given mean & standard deviation
end PragmARC.Real_Random_Ranges;
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
