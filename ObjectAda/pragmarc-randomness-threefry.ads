-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- The Threefry Random Number Generator
-- Based on the Threefish encryption algorithm, this is a fast, high-quality, long-period generator
-- Passes all of the Crush suite of tests (Big Crush), and said to be fastest generator that does so
--
-- The concept behind encryption-based or counter-based RNGs is that encryption is supposed to take a key and a plaintext and
-- create from them a ciphertext that looks random. The Threefish algorithm that Threefry is based on takes a 256-bit key and
-- 256-bit plaintext, and creates a 256-bit ciphertext. That ciphertext is then doled out 32 bits at a time
--
-- When you want random numbers, not ciphertext, it doesn't really matter what the plaintext is. So in normal use the plaintext
-- is treated as a 256-bit counter that starts at zero and is incremented by 1 each time the 8 random values generated from
-- the value have been returned by Random
--
-- Since there is such a large range of states (counters), multiple tasks can use the same key with different states to produce
-- independent streams of values without worrying about synchronization or overlapping seqeunces of values

-- History:
-- 2021 May 01     J. Carter     V2.2--Adhere to coding standard
-- 2021 Feb 01     J. Carter     V2.1--Use PragmARC.Encryption.Threefish
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Dec 15     J. Carter     V1.2--Added a description of the concept and use of counter-based RNGs, and added
--                                     Increment and Random (Key, State)
-- 2016 Oct 01     J. Carter     V1.1--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2013 Nov 01     J. Carter     V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Interfaces;
with PragmARC.Encryption.Threefish;

private with Ada.Finalization;

package PragmARC.Randomness.Threefry is
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_64 is Interfaces.Unsigned_64;

   type Generator is tagged limited private;
   -- Initial value: seeded with default seed of zero

   procedure Set_Seed (State : in out Generator; Seed : in Unsigned_32 := 0);
   -- Seeds State with Seed

   procedure Randomize (State : in out Generator);
   -- Seeds State with a value derived from the clock

   procedure Set_Key (State : in out Generator; Key : in PragmARC.Encryption.Threefish.Block);
   -- Seeds State with a key of Key
   -- Resets the counter of State to zero

   procedure Set_State (State : in out Generator; New_State : in PragmARC.Encryption.Threefish.Block);
   -- Seeds State with a state (counter) of New_State
   -- The key of State is unchanged

   function Random (State : in out Generator) return Unsigned_32;
   -- Returns a random value

   procedure Advance (State : in out Generator; By : in Unsigned_64);
   -- Advances the state of State by By; equivalent to exhausting the random values for By - 1 values of the counter

   function Random (Key : in PragmARC.Encryption.Threefish.Block; State : in PragmARC.Encryption.Threefish.Block)
   return Unsigned_32;
   -- Declares a Generator, calls Set_Key and Set_State with the given Key and State, and returns a call to Random on it
   -- This function is pure; it will return the same result for the same values of Key and State
private -- PragmARC.Randomness.Threefry
   use PragmARC.Encryption;

   type Output_List is array (1 .. 2 * Threefish.Num_Words) of Unsigned_32;

   type Generator is new Ada.Finalization.Limited_Controlled with record
      KS      : Threefish.Key_Schedule_Handle;
      State   : Threefish.Block;
      Output  : Output_List;
      Next    : Positive := 1; -- Output (Next) will be the next value returned by Random
   end record;

   overriding procedure Initialize (Object : in out Generator);
end PragmARC.Randomness.Threefry;
