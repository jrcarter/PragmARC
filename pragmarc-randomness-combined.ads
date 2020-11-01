-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Combines the Universal and KISS generators into one very high quality, very long period generator

-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.1--Removed Random_Range, Random_Int, and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2013 Aug 01     J. Carter     V1.0--Initial release

pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Randomness.KISS;

generic -- PragmARC.Randomness.Combined
   type Supplied_Real is digits <>; -- Requires a type with a mantissa of at least 24 bits
package PragmARC.Randomness.Combined is
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

   procedure Set_Seed (New_I : in Seed_Range_1                          := Default_I;
                       New_J : in Seed_Range_1                          := Default_J;
                       New_K : in Seed_Range_1                          := Default_K;
                       New_L : in Seed_Range_2                          := Default_L;
                       New_W : in PragmARC.Randomness.KISS.Raw_Value    := PragmARC.Randomness.KISS.Default_W;
                       New_X : in PragmARC.Randomness.KISS.Positive_Raw := PragmARC.Randomness.KISS.Default_X;
                       New_Y : in PragmARC.Randomness.KISS.Positive_Raw := PragmARC.Randomness.KISS.Default_Y;
                       New_Z : in PragmARC.Randomness.KISS.Positive_Raw := PragmARC.Randomness.KISS.Default_Z);
   -- Sets the seeds for the generator. I through J are seeds for the Universal generator;
   -- W through Z are seeds for the KISS generator

   procedure Randomize;
   -- Initializes the seeds for the generator to some derived from the clock

   subtype Uniform is Real range 0.0 .. Real'Adjacent (1.0, 0.0 );

   function Random return Uniform;
   -- Returns a random value
end PragmARC.Randomness.Combined;
