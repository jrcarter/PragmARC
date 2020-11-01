-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Definition of standard cards used in USA
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Cards.US is
   pragma Pure;

   type Suit_Id is (Diamond, Club, Heart, Spade);
   type Rank_Id is (Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King);

   type Card_Info is record
      Suit : Suit_Id := Suit_Id'First;
      Rank : Rank_Id := Rank_Id'First;
   end record;

   subtype Image_String is String (1 .. 2);

   function Image (Item : Card_Info) return Image_String;
   -- Returns the image of Item
   -- Format is "RS"; R=Rank, S=Suit
   -- For Two .. Nine, R is the corresponding digit; for Ace and Ten .. King, R is the 1st character (A, T, etc)
   -- S is the 1st character (D, C, H, or S)
   -- Thus, Image ( (Club, Seven) ) = "7C"
   --       Image ( (Spade, Jack) ) = "JS"
end PragmARC.Cards.US;
