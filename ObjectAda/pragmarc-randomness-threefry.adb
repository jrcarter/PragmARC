-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************

-- History:
-- 2021 May 01     J. Carter     V2.2--Adhere to coding standard
-- 2021 Feb 01     J. Carter     V2.1--Use PragmARC.Encryption.Threefish
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Dec 15     J. Carter     V1.2--Added Random (Key, State)
-- 2016 Oct 01     J. Carter     V1.1--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2013 Nov 01     J. Carter     v1.0--Initial release

with Ada.Calendar;
with Ada.Unchecked_Conversion;
with PragmARC.Date_Handler;

package body PragmARC.Randomness.Threefry is
   use type Unsigned_32;
   use type Unsigned_64;

   procedure Encrypt (State : in out Generator) with
      Post => State.Next = 1;
   -- Encrypts State.State with State.KS giving State.Output

   function To_Output is new Ada.Unchecked_Conversion (Source => Threefish.Block, Target => Output_List);

   procedure Set_Seed (State : in out Generator; Seed : in Unsigned_32 := 0) is
      -- Empty
   begin -- Set_Seed
      State.State := (others => 0);
      State.Next  := 1;
      Threefish.Create_Key_Schedule (Key => (Unsigned_64 (Seed), 0, 0, 0), Tweak => (0, 0), Key_Schedule => State.KS);

      if Seed > 0 then
         Encrypt (State => State);
      else
         State.Output := (16#0921_8EBD#,
                          16#E6C8_5537#,
                          16#5594_1F52#,
                          16#66D8_6105#,
                          16#4BD2_5E16#,
                          16#2824_34DC#,
                          16#EE29_EC84#,
                          16#6BD2_E40B#);
      end if;
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
      Key         : Threefish.Block;
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

      Key (0) := Unsigned_64 (Year) * Unsigned_64 (Month) * Unsigned_64 (Day) * Unsigned_64 (Hour);
      Key (1) := Key (0) * Unsigned_64 (Minute);
      Key (2) := Key (1) * Unsigned_64 (Seconds);
      Key (3) := Key (2) * Unsigned_64 (Hundredths);

      State.Set_Key (Key => Key);
   end Randomize;

   procedure Set_Key (State : in out Generator; Key : in PragmARC.Encryption.Threefish.Block) is
      -- Empty
   begin -- Set_Key
      Threefish.Create_Key_Schedule (Key => Key, Tweak => (0, 0), Key_Schedule => State.KS);
      Encrypt (State => State);
   end Set_Key;

   procedure Set_State (State : in out Generator; New_State : in PragmARC.Encryption.Threefish.Block) is
      -- Empty
   begin -- Set_State
      State.State := New_State;
      Encrypt (State => State);
   end Set_State;

   procedure Increment (Value : in out Threefish.Block; By : in Unsigned_64 := 1);
   -- Adds By to Value

   function Random (State : in out Generator) return Unsigned_32 is
      Result : constant Unsigned_32 := State.Output (State.Next);
   begin -- Random
      if State.Next < State.Output'Last then
         State.Next := State.Next + 1;

         return Result;
      end if;

      Increment (Value => State.State);
      Encrypt (State => State);
      State.Next := 2;

      return State.Output (1);
   end Random;

   procedure Advance (State : in out Generator; By : in Unsigned_64) is
      -- Empty
   begin -- Advance
      Increment (Value => State.State, By => By);
      Encrypt (State => State);
   end Advance;

   function Random (Key : in PragmARC.Encryption.Threefish.Block; State : in PragmARC.Encryption.Threefish.Block)
   return Unsigned_32 is
      Gen : Generator;
   begin -- Random
      Threefish.Create_Key_Schedule (Key => Key, Tweak => (0, 0), Key_Schedule => Gen.KS);
      Gen.State := State;
      Encrypt (State => Gen);

      return Gen.Random;
   end Random;

   procedure Initialize (Object : in out Generator) is
      -- Empty declarative part
   begin -- Initialize
      Object.Set_Seed;
   end Initialize;

   procedure Encrypt (State : in out Generator) is
      Last_Round : constant := 19;

      Result : Threefish.Block := State.State;
   begin -- Encrypt
      State.Next := 1;
      Threefish.Encrypt (Key_Schedule => State.KS, Text => Result, Last_Round => Last_Round);
      State.Output := To_Output (Result);
   end Encrypt;

   procedure Increment (Value : in out Threefish.Block; By : in Unsigned_64 := 1) is
      -- Empty declarative part
   begin -- Increment
      Value (0) := Value (0) + By;

      if By > Value (0) then
         Carry : for I in 1 .. 3 loop
            Value (I) := Value (I) + 1;

            exit Carry when Value (I) /= 0;
         end loop Carry;
      end if;
   end Increment;
end PragmARC.Randomness.Threefry;
