-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter          V1.0--Initial version
--
package body PragmARC.Randomness.Real_Ranges is
   function Random_Range (R : in Uniform; Min : in Real; Max : in Real) return Real is
      (R * (Max - Min) + Min);

   function Random_Int (R : in Uniform; Min : in Integer; Max : in Integer) return Integer is
      Value : constant Real := Random_Range (R, Real (Min), Real (Max) + 1.0);
      -- assert: Min <= Value < Max + 1
      -- assert: Min <= Floor (Value) <= Max
   begin -- Random_Int
      return Integer (Real'Floor (Value) );
   end Random_Int;

   function Normal (List : in Normal_List; Mean : in Real; Sigma : in Positive_Real) return Real is
      Sum : Real := 0.0;
   begin -- Normal
      Add : for V of List loop
         Sum := Sum + V;
      end loop Add;

      return Sigma * (Sum - 6.0) + Mean;
   end Normal;
end PragmARC.Randomness.Real_Ranges;
