-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Creates random values in a range from a random value R such that 0 <= R < 1
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Randomness.Real_Ranges
   type Supplied_Real is digits <>;
package PragmARC.Randomness.Real_Ranges is
   subtype Real is Supplied_Real'Base;
   subtype Uniform is Real range 0.0 .. Real'Adjacent (1.0, 0.0);

   function Random_Range (R : Uniform; Min : Real; Max : Real) return Real with
      Pre  => Min < Max,
      Post => Random_Range'Result in Min .. Max;
   -- Converts R into a value in Min .. Max

   function Random_Int (R : Uniform; Min : Integer; Max : Integer) return Integer with
      Pre  => Min < Max,
      Post => Random_Int'Result in Min .. Max;
   -- Converts R into a value in Min .. Max

   type Normal_List is array (1 .. 12) of Uniform;
   subtype Positive_Real is Real range Real'Adjacent (0.0, 1.0) .. Real'Last;

   function Normal (List : Normal_List; Mean : Real; Sigma : Positive_Real) return Real;
   -- Uses the random values in List to approximate a normally distributed random number with the given mean & standard deviation
end PragmARC.Randomness.Real_Ranges;
