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
package body PragmARC.Randomness.ISAAC is
   procedure Generate (Gen : in out Generator);
   -- Calculates a new set of values in Gen.Result

   use type U32;

   procedure Generate (Gen : in out Generator) is
      X : U32;
      Y : U32;

      subtype Rem_4 is U32 range 0 .. 3;
   begin -- Generate
      Gen.Count := Gen.Count + 1;
      Gen.Output := Gen.Output + Gen.Count;

      Calculate : for I in Gen.State'Range loop
         X := Gen.State (I);

         Gen.Acum := Gen.Acum xor (case Rem_4 (I rem 4) is
                                   when 0 => Interfaces.Shift_Left  (Gen.Acum, 13),
                                   when 1 => Interfaces.Shift_Right (Gen.Acum,  6),
                                   when 2 => Interfaces.Shift_Left  (Gen.Acum,  2),
                                   when 3 => Interfaces.Shift_Right (Gen.Acum, 16) );

         Gen.Acum := Gen.State ( (I + 128) rem 256) + Gen.Acum;
         Y := Gen.State (Interfaces.Shift_Right (X, 2) rem 256) + Gen.Acum + Gen.Output;
         Gen.State (I) := Y;
         Gen.Output := Gen.State (Interfaces.Shift_Right (Y, 10) rem 256) + X;
         Gen.Result (I) := Gen.Output;
      end loop Calculate;

      Gen.Next := 0;
   end Generate;

   procedure Set_Seed (Gen : in out Generator; Seed : in Seed_List) is
      -- Empty
   begin -- Set_Seed
      Gen.State  := State_List (Seed);
      Gen.Acum   := 0;
      Gen.Output := 0;
      Gen.Count  := 0;
      Generate (Gen => Gen);
   end Set_Seed;

   function Random (Gen : in out Generator) return U32 is
      -- Empty
   begin -- Random
      if Gen.Next not in Gen.Result'Range then
         Generate (Gen => Gen);
      end if;

      Gen.Next := Gen.Next + 1;

      return Gen.Result (Gen.Next - 1);
   end Random;
end PragmARC.Randomness.ISAAC;
