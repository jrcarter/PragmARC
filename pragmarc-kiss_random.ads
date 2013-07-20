-- Marsaglia'S KISS Random Number Generator

-- History:
-- 2013 Aug 01     J. Carter     v1.0--Initial release

with Interfaces;

package PragmARC.KISS_Random is
   subtype Raw_Value is Interfaces.Unsigned_32;
   subtype Positive_Raw is Raw_Value range 1 .. Raw_Value'Last;

   Default_W : constant := 916_191_069;
   Default_X : constant := 123_456_789;
   Default_Y : constant := 362_436_069;
   Default_Z : constant := 521_288_629;

   procedure Set_Seed (New_W : in Raw_Value    := Default_W;
                       New_X : in Positive_Raw := Default_X;
                       New_Y : in Positive_Raw := Default_Y;
                       New_Z : in Positive_Raw := Default_Z);
   -- Sets the seeds for the generator to those given
   -- The initial values for the seeds are the defaults listed

   procedure Randomize;
   -- Initializes the seeds for the generator to some derived from the clock

   function Raw return Raw_Value;
   -- Returns a random value

   function Random_Range (Min : in Raw_Value; Max : in Raw_Value) return Raw_Value;
   -- Returns a random value in the given range

   generic -- Real_Values
      type Supplied_Real is digits <>;
   package Real_Values is
      subtype Real is Supplied_Real'Base;

      function Random return Real;
      -- Returns a uniformly distributed random value in 0.0 .. 1.0 - Epsilon

      function Random_Range (Min : in Real; Max : in Real) return Real;
      -- Returns a random value in the given range

      function Normal (Mean : in Real; Sigma : in Real) return Real;
      -- Uses 12 random values to approximate a normally distributed random value with the give mean and standard deviation
   end Real_Values;
end PragmARC.KISS_Random;
