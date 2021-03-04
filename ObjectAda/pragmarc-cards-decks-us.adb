-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Cards.Decks.US is
   procedure Standard_Deck (Item : in out Deck_52) is
      -- Empty
   begin -- Standard_Deck
      Item.Make_Empty;

      All_Suits : for Suit in Cards.US.Suit_Id loop
         All_Ranks : for Rank in Cards.US.Rank_Id loop
            Item.Add (Item => (Suit => Suit, Rank => Rank) );
         end loop All_Ranks;
      end loop All_Suits;
   end Standard_Deck;
end PragmARC.Cards.Decks.US;
