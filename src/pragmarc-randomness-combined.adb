-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.2--Removed Random_Range, Random_Int, and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2016 Jun 01     J. Carter     V1.1--Changed comment for empty declarative part
-- 2013 Aug 01     J. Carter     V1.0--Initial release

with PragmARC.Randomness.Universal;

package body PragmARC.Randomness.Combined is
   package Universal is new PragmARC.Randomness.Universal (Supplied_Real => Real);

   subtype List_Index is PragmARC.Randomness.KISS.Raw_Value range 0 .. 255;

   type Number_List is array (List_Index) of Real;

   List       : Number_List := (others => Universal.Random);
   KISS_State : PragmARC.Randomness.KISS.Generator;

   procedure Set_Seed (New_I : in Seed_Range_1                      := Default_I;
                       New_J : in Seed_Range_1                      := Default_J;
                       New_K : in Seed_Range_1                      := Default_K;
                       New_L : in Seed_Range_2                      := Default_L;
                       New_W : in PragmARC.Randomness.KISS.Raw_Value    := PragmARC.Randomness.KISS.Default_W;
                       New_X : in PragmARC.Randomness.KISS.Positive_Raw := PragmARC.Randomness.KISS.Default_X;
                       New_Y : in PragmARC.Randomness.KISS.Positive_Raw := PragmARC.Randomness.KISS.Default_Y;
                       New_Z : in PragmARC.Randomness.KISS.Positive_Raw := PragmARC.Randomness.KISS.Default_Z)
   is
      -- Empty
   begin -- Set_Seed
      Universal.Set_Seed (New_I => New_I, New_J => New_J, New_K => New_K, New_L => New_L);
      KISS_State.Set_Seed (New_W => New_W, New_X => New_X, New_Y => New_Y, New_Z => New_Z);
      List := (others => Universal.Random);
   end Set_Seed;

   procedure Randomize is
      -- Empty
   begin -- Randomize
      Universal.Randomize;
      KISS_State.Randomize;
      List := (others => Universal.Random);
   end Randomize;

   function Random return Uniform is
      use type PragmARC.Randomness.KISS.Raw_Value;

      Index  : constant List_Index := KISS_State.Raw rem 256;
      Result : constant Real       := List (Index);
   begin -- Random
      List (Index) := Universal.Random;

      return Result;
   end Random;
end PragmARC.Randomness.Combined;
