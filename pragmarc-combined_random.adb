-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Oct 01     J. Carter     V1.2--Removed Random_Range, Random_Int, and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2016 Jun 01     J. Carter     V1.1--Changed comment for empty declarative part
-- 2013 Aug 01     J. Carter     V1.0--Initial release

with PragmARC.Universal_Random;

package body PragmARC.Combined_Random is
   package Universal is new PragmARC.Universal_Random (Supplied_Real => Real);

   subtype List_Index is PragmARC.KISS_Random.Raw_Value range 0 .. 255;

   type Number_List is array (List_Index) of Real;

   List       : Number_List := (others => Universal.Random);
   KISS_State : PragmARC.KISS_Random.Generator;

   procedure Set_Seed (New_I : in Seed_Range_1                      := Default_I;
                       New_J : in Seed_Range_1                      := Default_J;
                       New_K : in Seed_Range_1                      := Default_K;
                       New_L : in Seed_Range_2                      := Default_L;
                       New_W : in PragmARC.KISS_Random.Raw_Value    := PragmARC.KISS_Random.Default_W;
                       New_X : in PragmARC.KISS_Random.Positive_Raw := PragmARC.KISS_Random.Default_X;
                       New_Y : in PragmARC.KISS_Random.Positive_Raw := PragmARC.KISS_Random.Default_Y;
                       New_Z : in PragmARC.KISS_Random.Positive_Raw := PragmARC.KISS_Random.Default_Z)
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

   function Random return Real is
      use type PragmARC.KISS_Random.Raw_Value;

      Index  : constant List_Index := KISS_State.Raw rem 256;
      Result : constant Real       := List (Index);
   begin -- Random
      List (Index) := Universal.Random;

      return Result;
   end Random;
end PragmARC.Combined_Random;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
