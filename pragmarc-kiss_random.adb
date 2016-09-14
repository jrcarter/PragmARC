-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Oct 01     J. Carter     V1.3--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2016 Jun 01     J. Carter     V1.2--Changed comment for empty declarative part
-- 2013 Nov 01     J. Carter     V1.1--Eliminated an impossible case
-- 2013 Aug 01     J. Carter     V1.0--Initial release

with Ada.Calendar;
with PragmARC.Date_Handler;

package body PragmARC.KISS_Random is
   use type Raw_Value;

   procedure Set_Seed (State : in out Generator;
                       New_W : in     Raw_Value    := Default_W;
                       New_X : in     Positive_Raw := Default_X;
                       New_Y : in     Positive_Raw := Default_Y;
                       New_Z : in     Positive_Raw := Default_Z)
   is
      -- Empty
   begin -- Set_Seed
      State.W := New_W;
      State.X := New_X;
      State.Y := New_Y;
      State.Z := New_Z;
   end Set_Seed;

   procedure Randomize (State : in out Generator) is
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

      if Duration (Seconds) > Day_Seconds then
         Seconds := Seconds - 1;
      end if;

      Day_Seconds := Day_Seconds - Duration (Seconds);
      Seconds := Integer'Max (Seconds, 1);
      Hundredths := Integer'Max (Integer (100.0 * Day_Seconds), 1);

      Set_Seed (State => State,
                New_W => Raw_Value (Year * Hour),
                New_X => Raw_Value (Year * Minute),
                New_Y => Raw_Value (Year * Seconds),
                New_Z => Raw_Value (Year * Hundredths) );
   end Randomize;

   function Raw (State : in Generator) return Raw_Value is
      function ML (Value : in Raw_Value; Shift : in Natural) return Raw_Value;
      -- Returns Value xor Shift_Left (Value, Shift)

      function MR (Value : in Raw_Value; Shift : in Natural) return Raw_Value;
      -- Returns Value xor Shift_Right (Value, Shift)

      function ML (Value : in Raw_Value; Shift : in Natural) return Raw_Value is
         -- Empty
      begin -- ML
         return Value xor Interfaces.Shift_Left (Value, Shift);
      end ML;

      function MR (Value : in Raw_Value; Shift : in Natural) return Raw_Value is
         -- Empty
      begin -- MR
         return Value xor Interfaces.Shift_Right (Value, Shift);
      end MR;

      S : Generator renames State.Handle.State.all;
   begin -- Raw
      S.W := 30903 * (S.W and 65535) + Interfaces.Shift_Right (S.W, 16);
      S.X := 69069 * S.X + 1327217885;
      S.Y := ML (MR (ML (S.Y, 13), 17), 5);
      S.Z := 18000 * (S.Z and 65535) + Interfaces.Shift_Right (S.Z, 16);

      return S.X + S.Y + Interfaces.Shift_Left (S.Z, 16) + S.W;
   end Raw;
end PragmARC.KISS_Random;
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
