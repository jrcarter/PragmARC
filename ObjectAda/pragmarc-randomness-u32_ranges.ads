-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Returns a uniformly distributed integer in Min .. Max
-- Internally, it declares a type mod 2 ** 33, so this won't work with compilers that have
-- System.Max_Binary_Modulus < 2 ** 33
--
-- History:
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.0--Initial Version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Interfaces;
use type Interfaces.Unsigned_32;

generic -- PragmARC.Randomness.U32_Ranges
   type Generator (<>) is limited private;

   with function Random (G : in out Generator) return Interfaces.Unsigned_32;
function PragmARC.Randomness.U32_Ranges (G : in out Generator; Min : in Interfaces.Unsigned_32; Max : in Interfaces.Unsigned_32)
return Interfaces.Unsigned_32; -- with
--  Pre  => Min < Max,
--  Post => PragmARC.Randomness.U32_Ranges'Result in Min .. Max;
