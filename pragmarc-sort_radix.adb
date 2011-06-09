-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
procedure PragmARC.Sort_Radix (Set : in out Sort_Set) is
   Mask       : Element := 1;
   Zero       : Sort_Set (Set'range);
   One        : Sort_Set (Set'range);
   Zero_Index : Index;
   One_Index  : Index;
   Num_Zeroes : Element;
   Num_Ones   : Element;
begin -- PragmARC.Sort_Radix
   All_Bits : for I in 1 .. Element'Size loop
      Num_Zeroes := 0;
      Num_Ones   := 0;
      Zero_Index := Zero'First;
      One_Index  := One'First;

      All_Elements : for J in Set'range loop
         if (Set (J) and Mask) = 0 then -- Bit is zero
            Zero (Zero_Index) := Set (J);
            Num_Zeroes := Num_Zeroes + 1;
            Zero_Index := Index'Succ (Zero_Index);
         else -- Bit is one
            One (One_Index) := Set (J);
            Num_Ones := Num_Ones + 1;
            One_Index := Index'Succ (One_Index);
         end if;
      end loop All_Elements;

      if Num_Zeroes = 0 then
         Set := One;
      elsif Num_Ones = 0 then
         Set := Zero;
      else
         Set := Zero (Zero'First .. Index'Val (Index'Pos (Zero'First) + Num_Zeroes - 1) ) &
                One  (One'First  .. Index'Val (Index'Pos (One'First)  + Num_Ones   - 1) )
         ;
      end if;

      Mask := 2 * Mask; -- Move on to next bit
   end loop All_Bits;
end PragmARC.Sort_Radix;
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