-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.2--Sequences indexed by integers
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
procedure PragmARC.Sorting.Radix (Set : in out Sort_Set) is
   Mask       : Element := 1;
   Zero       : Sort_Set (Set'range);
   One        : Sort_Set (Set'range);
   Zero_Index : Index;
   One_Index  : Index;
   Num_Zeroes : Element;
   Num_Ones   : Element;
begin -- PragmARC.Sorting.Radix
   All_Bits : for I in 1 .. Element'Size loop
      Num_Zeroes := 0;
      Num_Ones   := 0;
      Zero_Index := Zero'First;
      One_Index  := One'First;

      All_Elements : for J in Set'Range loop
         if (Set (J) and Mask) = 0 then -- Bit is zero
            Zero (Zero_Index) := Set (J);
            Num_Zeroes := Num_Zeroes + 1;
            Zero_Index := Zero_Index + 1;
         else -- Bit is one
            One (One_Index) := Set (J);
            Num_Ones := Num_Ones + 1;
            One_Index := One_Index + 1;
         end if;
      end loop All_Elements;

      if Num_Zeroes = 0 then
         Set := One;
      elsif Num_Ones = 0 then
         Set := Zero;
      else
         Set := Zero (Zero'First .. Zero'First + Index (Num_Zeroes) - 1) &
                One  (One'First  .. One'First  + Index (Num_Ones)   - 1);
      end if;

      Mask := 2 * Mask; -- Move on to next bit
   end loop All_Bits;
end PragmARC.Sorting.Radix;
