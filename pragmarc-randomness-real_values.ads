-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- A package to obtain real-valued random numbers from generators that generate Unsigned_32 values (such as KISS and Threefry)
--
-- History:
-- 2025 Jul 01     J. Carter     V2.2--Use SPDX license format
-- 2020 Dec 01     J. Carter     V2.1--Expression function eliminates body
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
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

   function Random (State : in out Generator) return Uniform is
      (Real (Unsigned_Random (State) ) / Real (Interfaces.Unsigned_32'Modulus) );
   -- Converts Unsigned_Random (State) into a real value
end PragmARC.Randomness.Real_Values;
