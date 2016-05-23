-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.3--Changed comment for empty declarative part and formatting
-- 2003 Sep 01     J. Carter          V1.2--Improve Randomize
-- 2001 Feb 01     J. Carter          V1.1--Use Date_Handler in Randomize
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Calendar;
with PragmARC.Date_Handler;

use Ada;
package body PragmARC.Universal_Random is
   M3      : constant :=       97;
   Divisor : constant := 16777216.0;
   Init_C  : constant :=   362436.0 / Divisor;
   Cd      : constant :=  7654321.0 / Divisor;
   Cm      : constant := 16777213.0 / Divisor;

   subtype Range_1 is Integer range 0 .. M1 - 1;
   subtype Range_2 is Integer range 0 .. M2 - 1;
   subtype Range_3 is Integer range 1 .. M3;

   I  : Range_1;
   J  : Range_1;
   K  : Range_1;
   Ni : Integer;
   Nj : Integer;
   L  : Range_2;
   C  : Real;
   U  : array (Range_3) of Real;

   procedure Set_Seed (New_I : Seed_Range_1 := Default_I;
                       New_J : Seed_Range_1 := Default_J;
                       New_K : Seed_Range_1 := Default_K;
                       New_L : Seed_Range_2 := Default_L)
   is
      S : Real;
      T : Real;
      M : Range_1;
   begin -- Set_Seed
      I := New_I;
      J := New_J;
      K := New_K;
      L := New_L;
      Ni := Range_3'Last;
      Nj := Range_3'Last / 3 + 1;
      C := Init_C;

      Fill_U : for Ii in U'Range loop
         S := 0.0;
         T := 0.5;

         Calc_S : for Jj in 1 .. 24 loop
            M := ( ( (J * I) mod M1) * K) mod M1;
            I := J;
            J := K;
            K := M;
            L := (53 * L + 1) mod M2;

            if (L * M) mod 64 >= 32 then
               S := S + T;
            end if;

            T := 0.5 * T;
         end loop Calc_S;

         U (Ii) := S;
      end loop Fill_U;
   end Set_Seed;

   procedure Randomize is
      Year        : Calendar.Year_Number;
      Month       : Calendar.Month_Number;
      Day         : Calendar.Day_Number;
      Day_Seconds : Calendar.Day_Duration;
      Hour        : Natural;
      Minute      : Natural;
      Seconds     : Natural;
      Hundredths  : Natural;
   begin -- Randomize
      Date_Handler.Split (Date    => Calendar.Clock,
                          Year    => Year,
                          Month   => Month,
                          Day     => Day,
                          Hour    => Hour,
                          Minute  => Minute,
                          Seconds => Day_Seconds);

      Hour := Integer'Max (Hour, Seed_Range_1'First);
      -- assert: Hour in Seed_Range_1

      Minute := Integer'Max (Minute, Seed_Range_1'First);
      -- assert: Minute in Seed_Range_1

      Seconds := Natural (Day_Seconds);

      if Duration (Seconds) > Day_Seconds then
         Seconds := Seconds - 1;
      end if;
      -- assert: Seconds <= Day_Seconds

      Day_Seconds := Day_Seconds - Duration (Seconds);
      -- assert: Day_Seconds < 0.0

      Seconds := Integer'Max (Seconds, Seed_Range_1'First);
      -- assert: Seconds in Seed_Range_1

      Hundredths := Natural (100 * Day_Seconds);

      Set_Seed (New_I => Hour, New_J => Minute, New_K => Seconds, New_L => Hundredths);
   end Randomize;

   function Random return Real is
      Temp : Real;
   begin -- Random
      Temp := U (Ni) - U (Nj);
      if Temp < 0.0 then
         Temp := Temp + 1.0;
      end if;

      U (Ni) := Temp;

      Ni := Ni - 1;
      if Ni = 0 then
         Ni := Range_3'Last;
      end if;

      Nj := Nj - 1;
      if Nj = 0 then
         Nj := Range_3'Last;
      end if;

      C := C - Cd;
      if C < 0.0 then
         C := C + Cm;
      end if;

      Temp := Temp - C;
      if Temp < 0.0 then
         Temp := Temp + 1.0;
      end if;

      return Temp;
   end Random;

   function Random_Range (Min : Real; Max : Real) return Real is
      -- Empty
   begin -- Random_Range
      return Random * (Max - Min) + Min;
   end Random_Range;

   function Random_Int (Min : Integer; Max : Integer) return Integer is
      Min_Work : Integer := Integer'Min (Min, Max);
      Max_Work : Integer := Integer'Max (Min, Max);
      Value    : Real;
   begin -- Random_Int
      -- assert: Min_Work <= Max_Work

      Value := Random_Range (Real (Min_Work), Real (Max_Work) + 1.0);
      -- assert: Min_Work <= Value < Max_Work + 1
      -- assert: Min_Work <= Floor (Value) <= Max_Work

      return Integer (Real'Floor (Value) );
   end Random_Int;

   function Normal (Mean : Real; Sigma : Real) return Real is
      Sum : Real := 0.0;
   begin -- Normal
      Add : for I in 1 .. 12 loop
         Sum := Sum + Random;
      end loop Add;

      return Sigma * (Sum - 6.0) + Mean;
   end Normal;
begin -- PragmARC.Universal_Random
   Set_Seed; -- Initialize to default seeds
end PragmARC.Universal_Random;
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
