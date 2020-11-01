-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A package to obtain real-valued random numbers from generators that generate Unsigned_32 values (such as KISS and Threefry)
--
-- History:
-- 2020 OCT 15     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.1--Removed Random_Range and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2013 Nov 01     J. Carter     V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Interfaces;

generic -- PragmARC.Randomness.Real_Values
   type Supplied_Real is digits <>;
   type Generator (<>) is limited private;

   with function Unsigned_Random (State : in out Generator) return Interfaces.Unsigned_32;
package PragmARC.Randomness.Real_Values is
   subtype Real is Supplied_Real'Base;
   subtype Uniform is Real range 0.0 .. 1.0 - 2.0 ** (-32);

   function Random (State : in out Generator) return Uniform;
   -- Converts Unsigned_Random (State) into a real value
end PragmARC.Randomness.Real_Values;
