-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- The Threefry Random Number Generator
-- Based on the Threefish encryption algorithm, this is a fast, high-quality, long-period generator
-- Passes all of the Crush suite of tests (Big Crush), and said to be fastest generator that does so
--
-- The concept behind encryption-based or counter-based RNGs is that encryption is supposed to take a key and a plaintext and
-- create from them a ciphertext that looks random. The Threefish algorithm that Threefry is based on takes a 256-bit key and
-- 256-bit plaintext, and creates a 256-bit ciphertext. That ciphertext is then doled out 32 bits at a time.
--
-- When you want random numbers, not ciphertext, it doesn't really matter what the plaintext is. So in normal use the plaintext
-- is treated as a 256-bit counter that starts at zero and is incremented by 1 each time the 8 random values generated from
-- the value have been returned by Random.

-- History:
-- 2016 Dec 15     J. Carter     V1.2--Added a description of the concept and use of counter-based RNGs, and added
--                                     Increment and Random (Key, State)
-- 2016 Oct 01     J. Carter     V1.1--Pulled out Random_Range into PragmARC.Random_Ranges
-- 2013 Nov 01     J. Carter     V1.0--Initial release

with Interfaces;

private with Ada.Finalization;

package PragmARC.Threefry_Random is
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_64 is Interfaces.Unsigned_64;

   type Generator is tagged limited private;
   -- Initial value: seeded with default seed of zero

   procedure Set_Seed (State : in out Generator; Seed : in Unsigned_32 := 0);
   -- Seeds State with Seed

   procedure Randomize (State : in out Generator);
   -- Seeds State with a value derived from the clock

   type Unsigned_256 is array (1 .. 4) of Unsigned_64;
   -- Represents a 256-bit unsigned integer; index 1 is the least-significant digit; index 4 is the most-significant digit

   procedure Set_Key (State : in out Generator; Key : in Unsigned_256);
   -- Seeds State with a key of Key

   procedure Set_State (State : in out Generator; New_State : in Unsigned_256);
   -- Seeds State with a state (counter) of New_State

   function Random (State : Generator) return Unsigned_32;
   -- Returns a random value

   procedure Advance (State : in out Generator; By : in Unsigned_64);
   -- Advances the state of State by By; equivalent to exhausting the random values for By - 1 values of the counter

   procedure Increment (Value : in out Unsigned_256; By : in Unsigned_64 := 1);
   -- Adds By to Value

   function Random (Key : Unsigned_256; State : Unsigned_256) return Unsigned_32;
   -- Declares a Generator, calls Set_Key and Set_State with the given Key and State, and returns a call to Random on it
   -- This function is pure; it will return the same result for the same values of Key and State
   -- This is the function that results in counter-based RNGs being called parallel; multiple tasks can share a Key and partition
   -- the range of counter values among themselves
private -- PragmARC.Threefry_Random
   type Generator_Ptr is access all Generator;

   type Generator_Handle (State : Generator_Ptr) is limited null record;

   type Generator is new Ada.Finalization.Limited_Controlled with record
      Handle  : Generator_Handle (State => Generator'Unchecked_Access); -- The Rosen Trick
      Key     : Unsigned_256;
      State   : Unsigned_256;
      Output  : Unsigned_256;
      Counter : Natural;
   end record;

   procedure Initialize (Object : in out Generator);
end PragmARC.Threefry_Random;
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
