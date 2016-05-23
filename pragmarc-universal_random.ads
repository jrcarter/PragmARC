-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Universal random number generator
-- Passes "stringent" tests for randomness and independence
-- Modified by J. Carter from version given in Ada Letters 1988 Sep/Oct
-- Modifications:
--     Made generic on type returned
--     Changed names to Set_Seed and Random
--     Changed seed subtypes to enforce suitable selection of seeds
--     Added Randomize
-- Initially produces the sequence resulting from the default values specified
-- Can be set to produce any sequence by calling Set_Seed
-- Randomize uses Calendar to obtain 4 seeds
-- Random produces the next number in the sequence
-- Random_Range uses the generator to produce a random value in the given range
-- Random_Int uses the generator to produce a random integer in the given range
-- Normal uses 12 values from the generator to approximate a normally distributed random number with the given mean & standard
-- deviation
--
-- Ada 95 defines 2 standard random-number packages, Ada.Numerics.Float_Random & Ada.Numerics.Discrete_Random.
-- However, the random-number algorithm use by these packages is implementation defined. This package provides a portable generator
-- of known, good quality, should portability of the algorithm be a concern.
--
-- History:
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Universal_Random
   type Supplied_Real is digits <>;
package PragmARC.Universal_Random is
   subtype Real is Supplied_Real'Base;

   M1 : constant := 179;
   M2 : constant := M1 - 10;

   subtype Seed_Range_1 is Integer range 2 .. M1 - 1;
   subtype Seed_Range_2 is Integer range 0 .. M2 - 1;

   Default_I : constant Seed_Range_1 := 12;
   Default_J : constant Seed_Range_1 := 34;
   Default_K : constant Seed_Range_1 := 56;
   Default_L : constant Seed_Range_2 := 78;

   procedure Set_Seed (New_I : Seed_Range_1 := Default_I;
                       New_J : Seed_Range_1 := Default_J;
                       New_K : Seed_Range_1 := Default_K;
                       New_L : Seed_Range_2 := Default_L);

   procedure Randomize;

   function Random return Real;

   function Random_Range (Min : Real; Max : Real) return Real;

   function Random_Int (Min : Integer; Max : Integer) return Integer;

   function Normal (Mean : Real; Sigma : Real) return Real;
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
