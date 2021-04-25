-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides useful constants and functions not provided by Ada.Numerics
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Math with Pure is
   -- Useful constants:
   Sqrt_2 : constant := 1.41421_35623_73095_04880_16887_24209_69807_85696_71875; -- Sqrt (2.0)
   Sqrt_3 : constant := 1.73205_08075_68877_29352_74463_41505_87236_69428_05254; -- Sqrt (3.0)
   Log_2  : constant := 0.69314_71805_59945_30941_77321_21458_17656_80755_00134; -- Log ( 2.0)
   Log_10 : constant := 2.30258_50929_94045_68401_79914_54684_36420_76011_01488; -- Log (10.0)

   function GCD (Left : in Natural; Right : in Natural) return Natural;
   -- Greatest Common Divisor, using an iterative version of Euclid's algorithm

   function LCM (Left : in Natural; Right : in Natural) return Natural is -- Left * Right in Natural
      ( (Left * Right) / GCD (Left, Right) ) with
      Pre => Natural'Last / Left >= Right or else raise Constraint_Error;
   -- Least Common Multiple
end PragmARC.Math;
