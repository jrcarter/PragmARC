-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 Jan 01     J. Carter          V2.1--Removed Assign
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter          V1.3--Random_Int moved to PragmARC.Real_Random_Ranges
-- 2016 Jun 01     J. Carter          V1.2--Changed comment for empty declarative part
-- 2010 Oct 01     J. Carter          V1.1--Improved Shuffle
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Randomness.Real_Ranges;
with PragmARC.Randomness.Universal;
with System;

package body PragmARC.Cards.Decks.General is
   function Is_Empty (Item : Handle) return Boolean is
      (Item.Count <= 0);

   function Is_Full (Item : Handle) return Boolean is
      (Item.Count >= Item.Max_Cards);

   function Size (Item : Handle) return Natural is
      (Item.Count);

   procedure Shuffle (Item : in out Handle) is
      type Big is digits System.Max_Digits;

      package Random is new Randomness.Universal (Supplied_Real => Big);
      package Ranges is new Randomness.Real_Ranges (Supplied_Real => Big);

      Temp  : Card;
      Index : Positive;
   begin -- Shuffle
      Random.Randomize;

      Permute_All : for I in 1 .. Item.Count - 1 loop
         Temp := Item.Value (I);
         Index := Ranges.Random_Int (Random.Random, I, Item.Count);
         Item.Value (I) := Item.Value (Index);
         Item.Value (Index) := Temp;
      end loop Permute_All;
   end Shuffle;

   procedure Make_Empty (Item : in out Handle) is
      -- Empty
   begin -- Make_Empty
      Item.Count := 0;
   end Make_Empty;

   procedure Add (To : in out Handle; Item : in Card) is
      -- Empty
   begin -- Add
      To.Count := To.Count + 1;
      To.Value (To.Count) := Item;
   end Add;

   procedure Insert (Into : in out Handle; Item : in Card; Before : in Positive) is
      -- Empty
   begin -- Insert
      if Before > Into.Count then
         Add (Item => Item, To => Into);

         return;
      end if;

      Into.Value (Before + 1 .. Into.Count + 1) := Into.Value (Before .. Into.Count);
      Into.Value (Before) := Item;
      Into.Count := Into.Count + 1;
   end Insert;

   procedure Deal (From : in out Handle; To : out Card) is
      -- Empty
   begin -- Deal
      To := From.Value (From.Value'First);
      From.Count := From.Count - 1;

      if From.Count > 0 then
         From.Value (From.Value'First .. From.Count) := From.Value (From.Value'First + 1 .. From.Count + 1);
      end if;
   end Deal;

   procedure Remove (From : in out Handle; Position : in Positive; To : out Card) is
      -- Empty
   begin -- Remove
      To := From.Value (Position);
      From.Count := From.Count - 1;

      if From.Count > 0 then
         From.Value (Position .. From.Count) := From.Value (Position + 1 .. From.Count + 1);
      end if;
   end Remove;

   function Value (From : Handle; Position : Positive) return Card is
      (From.Value (Position) );

   function "=" (Left : Handle; Right : Handle) return Boolean is
      (Left.Count = Right.Count and then Left.Value (1 .. Left.Count) = Right.Value (1 .. Right.Count) );
end PragmARC.Cards.Decks.General;
