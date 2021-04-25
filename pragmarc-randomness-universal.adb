-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter          V1.4--Removed Random_Range, Random_Int, and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2016 Jun 01     J. Carter          V1.3--Changed comment for empty declarative part and formatting
-- 2003 Sep 01     J. Carter          V1.2--Improve Randomize
-- 2001 Feb 01     J. Carter          V1.1--Use Date_Handler in Randomize
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Calendar;
with PragmARC.Date_Handler;

package body PragmARC.Randomness.Universal is
   use Ada;

   M3      : constant :=       97;
   Divisor : constant := 16777216.0;
   Init_C  : constant :=   362436.0 / Divisor;
   Cd      : constant :=  7654321.0 / Divisor;
   Cm      : constant := 16777213.0 / Divisor;

   subtype Range_1 is Integer range 0 .. M1 - 1;
   subtype Range_2 is Integer range 0 .. M2 - 1;
   subtype Range_3 is Integer range 1 .. M3;

   type Real_List is array (Range_3) of Real;

   I  : Range_1;
   J  : Range_1;
   K  : Range_1;
   Ni : Integer;
   Nj : Integer;
   L  : Range_2;
   C  : Real;
   U  : Real_List;

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

   function Random return Uniform is
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
begin -- PragmARC.Randomness.Universal
   Set_Seed; -- Initialize to default seeds
end PragmARC.Randomness.Universal;
