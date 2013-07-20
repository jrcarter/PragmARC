-- History:
-- 2013 Aug 01     J. Carter     v1.0--Initial release

with Ada.Calendar;
with PragmARC.Date_Handler;

package body PragmARC.KISS_Random is
   W : Raw_Value := Default_W;
   X : Raw_Value := Default_X;
   Y : Raw_Value := Default_Y;
   Z : Raw_Value := Default_Z;

   use type Raw_Value;

   procedure Set_Seed (New_W : in Raw_Value    := Default_W;
                       New_X : in Positive_Raw := Default_X;
                       New_Y : in Positive_Raw := Default_Y;
                       New_Z : in Positive_Raw := Default_Z)
   is
      -- null;
   begin -- Set_Seed
      W := New_W;
      X := New_X;
      Y := New_Y;
      Z := New_Z;
   end Set_Seed;

   procedure Randomize is
      Year        : Ada.Calendar.Year_Number;
      Month       : Ada.Calendar.Month_Number;
      Day         : Ada.Calendar.Day_Number;
      Day_Seconds : Ada.Calendar.Day_Duration;
      Hour        : Natural;
      Minute      : Natural;
      Seconds     : Natural;
      Hundredths  : Natural;
   begin -- Randomize
      PragmARC.Date_Handler.Split (Date    => Ada.Calendar.Clock,
                                   Year    => Year,
                                   Month   => Month,
                                   Day     => Day,
                                   Hour    => Hour,
                                   Minute  => Minute,
                                   Seconds => Day_Seconds);

      Hour := Integer'Max (Hour, 1);
      Minute := Integer'Max (Minute, 1);
      Seconds := Integer (Day_Seconds);
      Day_Seconds := Day_Seconds - Duration (Seconds);
      Seconds := Integer'Max (Seconds, 1);
      Hundredths := Integer'Max (Integer (100.0 * Day_Seconds), 1);

      Set_Seed (New_W => Raw_Value (Year * Hour),
                New_X => Raw_Value (Year * Minute),
                New_Y => Raw_Value (Year * Seconds),
                New_Z => Raw_Value (Year * Hundredths) );
   end Randomize;

   function Raw return Raw_Value is
      function ML (Value : in Raw_Value; Shift : in Natural) return Raw_Value;
      -- Returns Value xor Shift_Left (Value, Shift)

      function MR (Value : in Raw_Value; Shift : in Natural) return Raw_Value;
      -- Returns Value xor Shift_Right (Value_Shift

      function ML (Value : in Raw_Value; Shift : in Natural) return Raw_Value is
         -- null;
      begin -- ML
         return Value xor Interfaces.Shift_Left (Value, Shift);
      end ML;

      function MR (Value : in Raw_Value; Shift : in Natural) return Raw_Value is
         -- null;
      begin -- MR
         return Value xor Interfaces.Shift_Right (Value, Shift);
      end MR;
   begin -- Raw
      W := 30903 * (W and 65535) + Interfaces.Shift_Right (W, 16);
      X := 69069 * X + 1327217885;
      Y := ML (MR (ML (Y, 13), 17), 5);
      Z := 18000 * (Z and 65535) + Interfaces.Shift_Right (Z, 16);

      return X + Y + Interfaces.Shift_Left (Z, 16) + W;
   end Raw;

   function Random_Range (Min : in Raw_Value; Max : in Raw_Value) return Raw_Value is
      Min_Work : constant Raw_Value := Raw_Value'Min (Min, Max);
      Max_Work : constant Raw_Value := Raw_Value'Max (Min, Max);

      Spread : constant Raw_Value := Max_Work - Min_Work + 1;
   begin -- Random_Range
      return Min_Work + Raw rem Spread;
   end Random_Range;

   package body Real_Values is
      function Random return Real is
         -- null;
      begin -- Random
         return Real (Raw) / Real (Raw_Value'Modulus);
      end Random;

      function Random_Range (Min : in Real; Max : in Real) return Real is
         -- null;
      begin -- Random_Range
         return Random * (Max - Min) + Min;
      end Random_Range;

      function Normal (Mean : in Real; Sigma : in Real) return Real is
         Sum : Real := 0.0;
      begin -- Normal
         Add : for I in 1 .. 12 loop
            Sum := Sum + Random;
         end loop Add;

         return Sigma * (Sum - 6.0) + Mean;
      end Normal;
   end Real_Values;
end PragmARC.KISS_Random;
