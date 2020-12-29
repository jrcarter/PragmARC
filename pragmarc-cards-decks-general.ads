-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A simplified cross between a list and a queue to be used as a deck of cards in card games
--
-- History:
-- 2021 Jan 01     J. Carter          V2.1--Removed limited and Assign
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2012 Dec 01     J. Carter          V1.2--Added Insert
-- 2002 Feb 01     J. Carter          V1.1--Removed pragma Pure
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Cards.Decks.General
   type Card is private; -- A deck of cards
package PragmARC.Cards.Decks.General is
   type Handle (Max_Cards : Positive) is tagged private;
   -- A Handle can hold at most Max_Cards cards
   -- A Handle is initially empty

   function Is_Empty (Item : Handle) return Boolean; -- Returns True if Item is empty; False otherwise
   function Is_Full  (Item : Handle) return Boolean; -- Returns True if Item is full;  False otherwise
   --
   -- Time: O(1)

   function Size (Item : Handle) return Natural;
   -- Returns the number of cards in Item
   --
   -- Time: O(1)

   procedure Shuffle (Item : in out Handle);
   -- Shuffles Item: Randomizes the order of the cards it contains
   --
   -- Time: O(N)

   procedure Make_Empty (Item : in out Handle);
   -- Turns Item into an empty Handle
   --
   -- Time: O(1)

   procedure Add (To : in out Handle; Item : in Card) with
      Pre  => not To.Is_Full or else raise Full,
      Post => not To.Is_Empty;
   -- Adds Item to the bottom of To
   --
   -- Time: O(1)

   procedure Insert (Into : in out Handle; Item : in Card; Before : in Positive) with
      Pre  => not Into.Is_Full or else raise Full,
      Post => not Into.Is_Empty;
   -- Inserts Item into Into before position Before. Item becomes the Card in Into at position Before
   -- If Before > Size (Into), Insert is the same as Add (Item => Item, To => Into)
   --
   -- Time: O(N)

   procedure Deal (From : in out Handle; To : out Card) with
      Pre  => not From.Is_Empty or else raise Empty,
      Post => not From.Is_Full;
   -- Removes the top card from From and assigns it to To
   -- Identical to Remove (From, 1, To);
   --
   -- Time: O(N)

   procedure Remove (From : in out Handle; Position : in Positive; To : out Card) with
      Pre  => Position in 1 .. From.Size or else raise Constraint_Error,
      Post => not From.Is_Full;
   -- Allows you to "deal" from any position in the Handle
   -- Position 1 is the top of the deck
   -- Position Size (From) is the bottom of the deck
   --
   -- Time: O(N)

   function Value (From : Handle; Position : Positive) return Card with
      Pre  => Position in 1 .. From.Size or else raise Constraint_Error;
   -- Provides the value of the card at Position in From, without removing it
   -- Position 1 is the top of the deck
   -- Position Size (From) is the bottom of the deck
   --
   -- Time: O(1)

   function "=" (Left : Handle; Right : Handle) return Boolean;
   -- Returns True if Left and Right contain the same deck; False otherwise
   --
   -- Time: O(N)
private -- PragmARC.Deck_Handler
   type Deck_Set is array (Positive range <>) of Card;

   type Handle (Max_Cards : Positive) is tagged record
      Count : Natural := 0;
      Value : Deck_Set (1 .. Max_Cards);
   end record;
end PragmARC.Cards.Decks.General;
