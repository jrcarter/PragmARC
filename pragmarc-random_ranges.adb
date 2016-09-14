-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Oct 01     J. Carter     V1.0--Initial Version
--
function PragmARC.Random_Ranges (G : Generator; Min : Interfaces.Unsigned_32; Max : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
is
   subtype Unsigned_32 is Interfaces.Unsigned_32;

   type U33 is mod 2 ** 33; -- 1 bit more than Unsigned_32, for calculating Max_Random

   Min_Work : constant Unsigned_32 := Unsigned_32'Min (Min, Max);
   Max_Work : constant Unsigned_32 := Unsigned_32'Max (Min, Max);

   use type Unsigned_32;

   Spread : constant Unsigned_32 := Max_Work - Min_Work + 1;
   S33    : constant U33         := U33 (Spread);

   Max_Random : constant Unsigned_32 := Interfaces.Unsigned_32 (S33 * (Unsigned_32'Modulus / S33) - 1);

   Value : Unsigned_32;
begin -- PragmARC.Random_Ranges
   Get_Value : loop
      Value := Random (G);

      exit Get_Value when Value <= Max_Random;
   end loop Get_Value;

   return Min_Work + Value rem Spread;
end PragmARC.Random_Ranges;
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
