-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- The Threefry Random Number Generator
-- Based on the Threefish encryption algorithm, this is a fast, high-quality, long-period generator
-- Passes all of the Crush suite of tests (Big Crush), and said to be fastest generator that does so

-- History:
-- 2013 Nov 01     J. Carter     v1.0--Initial release

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
   -- Seeds State with a state of New_State

   function Random (State : in Generator) return Unsigned_32;
   -- Returns a random value

   procedure Advance (State : in out Generator; By : in Unsigned_64);
   -- Advances the state of State by By; equivalent to By calls to Random

   function Random_Range (State : in Generator; Min : in Unsigned_32; Max : in Unsigned_32) return Unsigned_32;
   -- Returns a random value in the given range
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
