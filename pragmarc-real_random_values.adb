-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************

-- History:
-- 2013 Nov 01     J. Carter     v1.0--Initial release

package body PragmARC.Real_Random_Values is
   function Random (State : in Generator) return Real is
      -- Empty declarative part
   begin -- Random
      return Real (Unsigned_Random (State) ) / Real (Interfaces.Unsigned_32'Modulus);
   end Random;

   function Random_Range (State : in Generator; Min : in Real; Max : in Real) return Real is
      -- Empty declarative part
   begin -- Random_Range
      return Random (State) * (Max - Min) + Min;
   end Random_Range;

   function Normal (State : in Generator; Mean : in Real; Sigma : in Real) return Real is
      Sum : Real := 0.0;
   begin -- Normal
      Add : for I in 1 .. 12 loop
         Sum := Sum + Random (State);
      end loop Add;

      return Sigma * (Sum - 6.0) + Mean;
   end Normal;
end PragmARC.Real_Random_Values;
