-- Combines the Universal and KISS generators into one very high quality, very long period generator

-- History:
-- 2013 Aug 01     J. Carter     v1.0--Initial release

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

   function Random_Range (Min : Real; Max : Real) return Real;
   -- Returns a random value in the given range

   function Random_Int (Min : Integer; Max : Integer) return Integer;
   -- Returns a random integer in the given range

   function Normal (Mean : Real; Sigma : Real) return Real;
   -- Uses 12 random values to approximate a normally distributed random value with the give mean and standard deviation
end PragmARC.Combined_Random;
