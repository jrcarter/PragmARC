-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
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

   type Generator is tagged limited private;

   procedure Set_Seed (State : in out Generator;
                       New_W : in     Raw_Value    := Default_W;
                       New_X : in     Positive_Raw := Default_X;
                       New_Y : in     Positive_Raw := Default_Y;
                       New_Z : in     Positive_Raw := Default_Z);
   -- Sets the seeds for State to those given
   -- The initial values for a Generator are the defaults listed

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

   type Generator is tagged limited record
      Handle : Generator_Handle (State => Generator'Unchecked_Access); -- The Rosen Trick
      W      : Raw_Value := Default_W;
      X      : Raw_Value := Default_X;
      Y      : Raw_Value := Default_Y;
      Z      : Raw_Value := Default_Z;
   end record;
end PragmARC.KISS_Random;
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
