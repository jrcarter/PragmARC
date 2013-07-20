-- History:
-- 2013 Aug 01     J. Carter     v1.0--Initial release

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
      -- null;
   begin -- Set_Seed
      Universal.Set_Seed (New_I => New_I, New_J => New_J, New_K => New_K, New_L => New_L);
      PragmARC.KISS_Random.Set_Seed (State => KISS_State, New_W => New_W, New_X => New_X, New_Y => New_Y, New_Z => New_Z);
      List := (others => Universal.Random);
   end Set_Seed;

   procedure Randomize is
      -- null;
   begin -- Randomize
      Universal.Randomize;
      PragmARC.KISS_Random.Randomize (State => KISS_State);
      List := (others => Universal.Random);
   end Randomize;

   function Random return Real is
      use type PragmARC.KISS_Random.Raw_Value;

      Index  : constant List_Index := PragmARC.KISS_Random.Raw (KISS_State) rem 256;
      Result : constant Real       := List (Index);
   begin -- Random
      List (Index) := Universal.Random;

      return Result;
   end Random;

   function Random_Range (Min : Real; Max : Real) return Real is
      -- null;
   begin -- Random_Range
      return Random * (Max - Min) + Min;
   end Random_Range;

   function Random_Int (Min : Integer; Max : Integer) return Integer is
      Min_Work : constant Integer := Integer'Min (Min, Max);
      Max_Work : constant Integer := Integer'Max (Min, Max);

      Value : Real;
   begin -- Random_Int
      -- assert: Min_Work <= Max_Work

      Value := Random_Range (Real (Min_Work), Real (Max_Work) + 1.0);
      -- assert: Min_Work <= Value < Max_Work + 1
      -- assert: Min_Work <= Floor (Value) <= Max_Work

      return Integer (Real'Floor (Value) );
   end Random_Int;

   function Normal (Mean : Real; Sigma : Real) return Real is
      Sum : Real := 0.0;
   begin -- Normal
      Add : for I in 1 .. 12 loop
         Sum := Sum + Random;
      end loop Add;

      return Sigma * (Sum - 6.0) + Mean;
   end Normal;
end PragmARC.Combined_Random;
