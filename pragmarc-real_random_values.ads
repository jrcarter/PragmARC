-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- A package to obtain real-valued random numbers from generators that generate Unsigned_32 values (such as KISS and Threefry)

-- History:
-- 2013 Nov 01     J. Carter     v1.0--Initial release

with Interfaces;

generic -- PragmARC.Real_Random_Values
   type Supplied_Real is digits <>;
   type Generator (<>) is limited private;

   with function Unsigned_Random (State : in Generator) return Interfaces.Unsigned_32;
package PragmARC.Real_Random_Values is
   subtype Real is Supplied_Real'Base;

   function Random (State : in Generator) return Real;
   -- Returns a uniformly distributed random value in 0.0 .. 1.0 - 2 ** (-32)

   function Random_Range (State : in Generator; Min : in Real; Max : in Real) return Real;
   -- Returns a random value in the given range

   function Normal (State : in Generator; Mean : in Real; Sigma : in Real) return Real;
   -- Uses 12 random values to approximate a normally distributed random value with the given mean and standard deviation
end PragmARC.Real_Random_Values;
