-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
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
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter          V1.2--Removed Random_Range, Random_Int, and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Randomness.Universal
   type Supplied_Real is digits <>;
package PragmARC.Randomness.Universal is
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

   procedure Set_Seed (New_I : Seed_Range_1 := Default_I;
                       New_J : Seed_Range_1 := Default_J;
                       New_K : Seed_Range_1 := Default_K;
                       New_L : Seed_Range_2 := Default_L);

   procedure Randomize;

   subtype Uniform is Real range 0.0 .. Real'Adjacent (1.0, 0.0 );

   function Random return Uniform;
end PragmARC.Randomness.Universal;
