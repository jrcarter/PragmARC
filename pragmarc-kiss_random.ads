-- Marsaglia'S KISS Random Number Generator
-- Alogirithm taken from www.fortran.com/kiss.f90

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

   type Generator is limited private;

   procedure Set_Seed (State : in out Generator;
                       New_W : in     Raw_Value    := Default_W;
                       New_X : in     Positive_Raw := Default_X;
                       New_Y : in     Positive_Raw := Default_Y;
                       New_Z : in     Positive_Raw := Default_Z);
   -- Sets the seeds for State to those given
   -- The initial values a Generator are the defaults listed

   procedure Randomize (State : in out Generator);
   -- Initializes the seeds for State to some derived from the clock

   function Raw (State : in Generator) return Raw_Value;
   -- Returns a random value

   function Random_Range (State : in Generator; Min : in Raw_Value; Max : in Raw_Value) return Raw_Value;
   -- Returns a random value in the given range

   generic -- Real_Values
      type Supplied_Real is digits <>;
   package Real_Values is
      subtype Real is Supplied_Real'Base;

      function Random (State : in Generator) return Real;
      -- Returns a uniformly distributed random value in 0.0 .. 1.0 - Epsilon

      function Random_Range (State : in Generator; Min : in Real; Max : in Real) return Real;
      -- Returns a random value in the given range

      function Normal (State : in Generator; Mean : in Real; Sigma : in Real) return Real;
      -- Uses 12 random values to approximate a normally distributed random value with the give mean and standard deviation
   end Real_Values;
private -- PragmARC.KISS_Random
   type Generator_Ptr is access all Generator;

   type Generator_Handle (State : Generator_Ptr) is limited null record;

   type Generator is limited record
      Handle : Generator_Handle (State => Generator'Unchecked_Access); -- The Rosen Trick
      W      : Raw_Value := Default_W;
      X      : Raw_Value := Default_X;
      Y      : Raw_Value := Default_Y;
      Z      : Raw_Value := Default_Z;
   end record;
end PragmARC.KISS_Random;
