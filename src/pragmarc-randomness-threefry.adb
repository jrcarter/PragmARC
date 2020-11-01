-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************

-- History:
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Dec 15     J. Carter     V1.2--Added Random (Key, State)
-- 2016 Oct 01     J. Carter     V1.1--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2013 Nov 01     J. Carter     v1.0--Initial release

with Ada.Calendar;
with PragmARC.Date_Handler;

package body  PragmARC.Randomness.Threefry is
   use type Unsigned_32;
   use type Unsigned_64;

   procedure Encrypt_State (State : in out Generator);
   -- Performs the encryption processing of State to give 8 more output values

   procedure Set_Seed (State : in out Generator; Seed : in Unsigned_32 := 0) is
      -- Empty declarative part
   begin -- Set_Seed
      State.Key := (1 => Unsigned_64 (Seed), others => 0);
      State.State := (others => 0);
      State.Counter := 0;

      if Seed > 0 then
         Encrypt_State (State => State);
      else
         State.Output := (1 => 16#0921_8EBD_E6C8_5537#, 2 => 16#5594_1F52_66D8_6105#,
                          3 => 16#4BD2_5E16_2824_34DC#, 4 => 16#EE29_EC84_6BD2_E40B#);
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
      Key         : Unsigned_256;
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

      Key (1) := Unsigned_64 (Year) * Unsigned_64 (Month) * Unsigned_64 (Day) * Unsigned_64 (Hour);
      Key (2) := Key (1) * Unsigned_64 (Minute);
      Key (3) := Key (2) * Unsigned_64 (Seconds);
      Key (4) := Key (3) * Unsigned_64 (Hundredths);

      State.Set_Key (Key => Key);
   end Randomize;

   procedure Set_Key (State : in out Generator; Key : in Unsigned_256) is
      -- Empty declarative part
   begin -- Set_Key
      State.Key := Key;
      State.Counter := 0;
      Encrypt_State (State => State);
   end Set_Key;

   procedure Set_State (State : in out Generator; New_State : in Unsigned_256) is
      -- Empty declarative part
   begin -- Set_State
      State.State := New_State;
      State.Counter := 0;
      Encrypt_State (State => State);
   end Set_State;

   function Random (State : in out Generator) return Unsigned_32 is
      Index : constant Natural := State.Counter / 2 + 1;
   begin -- Random
      if State.Counter < 8 then
         State.Counter := State.Counter + 1;

         if State.Counter rem 2 /= 0 then
            return Unsigned_32 (State.Output (Index) rem Unsigned_32'Modulus);
         end if;

         return Unsigned_32 (State.Output (Index) / Unsigned_32'Modulus);
      end if;

      Increment (Value => State.State);
      Encrypt_State (State => State);
      State.Counter := 1;

      return Unsigned_32 (State.Output (1) rem Unsigned_32'Modulus);
   end Random;

   procedure Advance (State : in out Generator; By : in Unsigned_64) is
      -- Empty declarative part
   begin -- Advance
      Increment (Value => State.State, By => By);
      State.Counter := 0;
      Encrypt_State (State => State);
   end Advance;

   function Random (Key : Unsigned_256; State : Unsigned_256) return Unsigned_32 is
      Gen : Generator;
   begin -- Random
      Gen.Key := Key;
      Gen.State := State;
      Gen.Counter := 0;
      Encrypt_State (State => Gen);

      return Gen.Random;
   end Random;

   procedure Initialize (Object : in out Generator) is
      -- Empty declarative part
   begin -- Initialize
      Object.Set_Seed;
   end Initialize;

   procedure Encrypt_State (State : in out Generator) is
      procedure Mix_Key (X0 : in out Unsigned_64; X1 : in out Unsigned_64; Rotate_X : in Natural;
                         Z0 : in out Unsigned_64; Z1 : in out Unsigned_64; Rotate_Z : in Natural;
                         K0 : in Unsigned_64; K1 : in Unsigned_64; L0 : in Unsigned_64; L1 : in Unsigned_64);
      -- Encryption step for mixing state and key together

      procedure Mix_Double (X0 : in out Unsigned_64; X1 : in out Unsigned_64; Rotate_X : in Natural;
                            Z0 : in out Unsigned_64; Z1 : in out Unsigned_64; Rotate_Z : in Natural);
      -- Encryption step for mixing digits of state together

      procedure Mix_Key (X0 : in out Unsigned_64; X1 : in out Unsigned_64; Rotate_X : in Natural;
                         Z0 : in out Unsigned_64; Z1 : in out Unsigned_64; Rotate_Z : in Natural;
                         K0 : in Unsigned_64; K1 : in Unsigned_64; L0 : in Unsigned_64; L1 : in Unsigned_64)
      is
         -- Empty declarative part
      begin -- Mix_Key
         X1 := X1 + K1;
         Z1 := Z1 + L1;
         X0 := X0 + X1 + K0;
         Z0 := Z0 + Z1 + L0;
         X1 := Interfaces.Rotate_Left (X1, Rotate_X);
         Z1 := Interfaces.Rotate_Left (Z1, Rotate_Z);
         X1 := X1 xor X0;
         Z1 := Z1 xor Z0;
      end Mix_Key;

      procedure Mix_Double (X0 : in out Unsigned_64; X1 : in out Unsigned_64; Rotate_X : in Natural;
                            Z0 : in out Unsigned_64; Z1 : in out Unsigned_64; Rotate_Z : in Natural)
      is
         -- Empty declarative part
      begin -- Mix_Double
         X0 := X0 + X1;
         Z0 := Z0 + Z1;
         X1 := Interfaces.Rotate_Left (X1, Rotate_X);
         Z1 := Interfaces.Rotate_Left (Z1, Rotate_Z);
         X1 := X1 xor X0;
         Z1 := Z1 xor Z0;
      end Mix_Double;

      Work_Key  : constant Unsigned_256 := State.Key;
      Key_Extra : constant Unsigned_64  :=
         16#1BD1_1BDA_A9FC_1A22# xor State.Key (1) xor State.Key (2) xor State.Key (3) xor State.Key (4);

      Work_State : Unsigned_256 := State.State;
   begin -- Encrypt_State
      Mix_Key (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 14,
               Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 16,
               K0 => Work_Key (1), K1 => Work_Key (2), L0 => Work_Key (3), L1 => Work_Key (4) );
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 52,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 57);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 23,
                  Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 40);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X =>  5,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 37);
      Mix_Key (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 25,
               Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 33,
               K0 => Work_Key (2), K1 => Work_Key (3), L0 => Work_Key (4), L1 => Key_Extra + 1);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 46,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 12);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 58,
                  Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 22);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 32,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 32);
      Mix_Key (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 14,
               Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 16,
               K0 => Work_Key (3), K1 => Work_Key (4), L0 => Key_Extra, L1 => Work_Key (1) + 2);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 52,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 57);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 23,
                  Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 40);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X =>  5,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 37);
      Mix_Key (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 25,
               Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 33,
               K0 => Work_Key (4), K1 => Key_Extra, L0 => Work_Key (1), L1 => Work_Key (2) + 3);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 46,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 12);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 58,
                  Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 22);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 32,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 32);
      Mix_Key (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 14,
               Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 16,
               K0 => Key_Extra, K1 => Work_Key (1), L0 => Work_Key (2), L1 => Work_Key (3) + 4);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X => 52,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 57);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (2), Rotate_X => 23,
                  Z0 => Work_State (3), Z1 => Work_State (4), Rotate_Z => 40);
      Mix_Double (X0 => Work_State (1), X1 => Work_State (4), Rotate_X =>  5,
                  Z0 => Work_State (3), Z1 => Work_State (2), Rotate_Z => 37);

      Sum : for I in State.Output'Range loop
         State.Output (I) := Work_State (I) + Work_Key (I);
      end loop Sum;

      State.Output (4) := State.Output (4) + 5;
   end Encrypt_State;

   procedure Increment (Value : in out Unsigned_256; By : in Unsigned_64 := 1) is
      -- Empty declarative part
   begin -- Increment
      Value (1) := Value (1) + By;

      if By > Value (1) then
         Carry : for I in 2 .. 4 loop
            Value (I) := Value (I) + 1;

            exit Carry when Value (I) /= 0;
         end loop Carry;
      end if;
   end Increment;
end PragmARC.Randomness.Threefry;
