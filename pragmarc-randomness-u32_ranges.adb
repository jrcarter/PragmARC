-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2025 Jul 01     J. Carter     V2.1--Use SPDX license format
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.0--Initial Version
--
function PragmARC.Randomness.U32_Ranges (G : in out Generator; Min : in Interfaces.Unsigned_32; Max : in Interfaces.Unsigned_32)
return Interfaces.Unsigned_32 is
   subtype Unsigned_32 is Interfaces.Unsigned_32;

   type U33 is mod 2 ** 33; -- 1 bit more than Unsigned_32, for calculating Max_Random

   Spread : constant Unsigned_32 := Max - Min + 1;
   S33    : constant U33         := U33 (Spread);

   Max_Random : constant Unsigned_32 := Interfaces.Unsigned_32 (S33 * (Unsigned_32'Modulus / S33) - 1);

   Value : Unsigned_32;
begin -- PragmARC.Randomness.U32_Ranges
   Get_Value : loop
      Value := Random (G);

      exit Get_Value when Value <= Max_Random;
   end loop Get_Value;

   return Min + Value rem Spread;
end PragmARC.Randomness.U32_Ranges;
