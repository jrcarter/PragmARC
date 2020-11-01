-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- An instantiation of PragmARC.Cards.Decks.General for the cards defined in PragmARC.Cards.US
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Cards.Decks.General;
with PragmARC.Cards.US;

package PragmARC.Cards.Decks.US is
   package Deck is new Decks.General (Card => Cards.US.Card_Info);

   subtype Deck_52 is Deck.Handle (Max_Cards => 52);

   procedure Standard_Deck (Item : in out Deck_52) with
      Post => Item.Is_Full; -- Converts Deck into the standard US 52-card deck in suit & rank order
end PragmARC.Cards.Decks.US;
