-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2010 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2014 May 01     J. Carter          V1.3--Improved Shuffle more
-- 2010 Oct 01     J. Carter          V1.2--Improved Shuffle
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Universal_Random;
with System;

package body PragmARC.Deck_Handler is
   function Is_Empty (Item : Handle) return Boolean is
      -- null;
   begin -- Is_Empty
      return Item.Count <= 0;
   end Is_Empty;

   function Is_Full (Item : Handle) return Boolean is
      -- null;
   begin -- Is_Full
      return Item.Count >= Item.Max_Cards;
   end Is_Full;

   function Size (Item : Handle) return Natural is
      -- null;
   begin -- Size
      return Item.Count;
   end Size;

   procedure Shuffle (Item : in out Handle) is
      type Big is digits System.Max_Digits;

      package Random is new Universal_Random (Supplied_Real => Big);

      Temp  : Card;
      Index : Positive;
   begin -- Shuffle
      Random.Randomize;

      Permute_All : for I in 1 .. Item.Count - 1 loop
         Temp := Item.Value (I);
         Index := Random.Random_Int (I + 1, Item.Count);
         Item.Value (I) := Item.Value (Index);
         Item.Value (Index) := Temp;
      end loop Permute_All;
   end Shuffle;

   procedure Make_Empty (Item : in out Handle) is
      -- null;
   begin -- Make_Empty
      Item.Count := 0;
   end Make_Empty;

   procedure Add (Item : in Card; To : in out Handle) is
      -- null;
   begin -- Add
      if To.Count >= To.Max_Cards then
         raise Full;
      end if;

      To.Count := To.Count + 1;
      To.Value (To.Count) := Item;
   end Add;

   procedure Insert (Item : in Card; Into : in out Handle; Before : in Positive) is
      -- null;
   begin -- Insert
      if Into.Count >= Into.Max_Cards then
         raise Full;
      end if;

      if Before > Into.Count then
         Add (Item => Item, To => Into);

         return;
      end if;

      Into.Value (Before + 1 .. Into.Count + 1) := Into.Value (Before .. Into.Count);
      Into.Value (Before) := Item;
      Into.Count := Into.Count + 1;
   end Insert;

   procedure Deal (From : in out Handle; To : out Card) is
      -- null;
   begin -- Deal
      if From.Count <= 0 then
         raise Empty;
      end if;

      To := From.Value (From.Value'First);
      From.Count := From.Count - 1;

      if From.Count > 0 then
         From.Value (From.Value'First .. From.Count) := From.Value (From.Value'First + 1 .. From.Count + 1);
      end if;
   end Deal;

   procedure Remove (From : in out Handle; Position : in Positive; To : out Card) is
      -- null;
   begin -- Remove
      if Position > From.Count then
         raise Empty;
      end if;

      To := From.Value (Position);
      From.Count := From.Count - 1;

      if From.Count > 0 then
         From.Value (Position .. From.Count) := From.Value (Position + 1 .. From.Count + 1);
      end if;
   end Remove;

   function Value (From : Handle; Position : Positive) return Card is
      -- null;
   begin -- Value
      if Position > From.Count then
         raise Empty;
      end if;

      return From.Value (Position);
   end Value;

   procedure Assign (To : in out Handle; From : in Handle) is
      -- null;
   begin -- Assign
      if From.Count > To.Max_Cards then
         raise Full;
      end if;

      To.Count := From.Count;
      To.Value (1 .. To.Count) := From.Value (1 .. From.Count);
   end Assign;

   function "=" (Left : Handle; Right : Handle) return Boolean is
      -- null;
   begin -- "="
      return Left.Count = Right.Count and then Left.Value (1 .. Left.Count) = Right.Value (1 .. Right.Count);
   end "=";
end PragmARC.Deck_Handler;
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
