-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Combines the Universal and KISS generators into one very high quality, very long period generator

-- History:
-- 2016 Oct 01     J. Carter     V1.1--Removed Random_Range, Random_Int, and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2013 Aug 01     J. Carter     V1.0--Initial release

with PragmARC.KISS_Random;

generic -- PragmARC.Combined_Random
   type Supplied_Real is digits <>; -- Requires a type with a mantissa of at least 24 bits
package PragmARC.Combined_Random is
   subtype Real is Supplied_Real'Base;
   pragma Assert (Real'Machine_Mantissa >= 24);

   M1 : constant := 179;
   M2 : constant := M1 - 10;

   subtype Seed_Range_1 is Integer range 2 .. M1 - 1;
   subtype Seed_Range_2 is Integer range 0 .. M2 - 1;

   Default_I : constant Seed_Range_1 := 12;
   Default_J : constant Seed_Range_1 := 34;
   Default_K : constant Seed_Range_1 := 56;
   Default_L : constant Seed_Range_2 := 78;

   procedure Set_Seed (New_I : in Seed_Range_1                      := Default_I;
                       New_J : in Seed_Range_1                      := Default_J;
                       New_K : in Seed_Range_1                      := Default_K;
                       New_L : in Seed_Range_2                      := Default_L;
                       New_W : in PragmARC.KISS_Random.Raw_Value    := PragmARC.KISS_Random.Default_W;
                       New_X : in PragmARC.KISS_Random.Positive_Raw := PragmARC.KISS_Random.Default_X;
                       New_Y : in PragmARC.KISS_Random.Positive_Raw := PragmARC.KISS_Random.Default_Y;
                       New_Z : in PragmARC.KISS_Random.Positive_Raw := PragmARC.KISS_Random.Default_Z);
   -- Sets the seeds for the generator. I through J are seeds for the Universal generator;
   -- W through Z are seeds for the KISS generator

   procedure Randomize;
   -- Initializes the seeds for the generator to some derived from the clock

   function Random return Real;
   -- Returns a uniformly distributed random value in 0.0 .. 1.0 - Epsilon
end PragmARC.Combined_Random;
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
