-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- The ISAAC cryptographically secure random-number generator (http://burtleburtle.net/bob/rand/isaac.html)
--
-- History:
-- 2023 May 15     J. Carter     V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Interfaces;

package PragmARC.Randomness.ISAAC is
   subtype U32 is Interfaces.Unsigned_32;

   type U32_List is array (U32 range <>) of U32;
   subtype Seed_List is U32_List (1 .. 256);

   type Generator is tagged limited private;
   -- The default value for Generator (equivalent to calling Set_Seed with a seed of all zeros) produces a sequence that begins
   -- with 132 ones, and doesn't begin to look random until 368 values have been produced

   procedure Set_Seed (Gen : in out Generator; Seed : in Seed_List);
   -- Sets the internal state of Gen to Seed

   function Random (Gen : in out Generator) return U32;
   -- Returns a random value
private -- PragmARC.Randomness.ISAAC
   subtype State_List is U32_List (0 .. 255);

   type Generator is tagged limited record
      State  : State_List := (others => 0);
      Result : State_List := (others => 0);
      Acum   : U32 :=   0;
      Output : U32 :=   0;
      Count  : U32 :=   0;
      Next   : U32 := 256;
   end record;
end PragmARC.Randomness.ISAAC;
