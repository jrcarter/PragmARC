-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Definition of standard cards used in USA
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package PragmARC.US_Card is
   pragma Pure;

   type Suit_Id is (Diamond, Club, Heart, Spade);
   type Rank_Id is (Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King);

   type Card_Handle is private;

   function Make (Suit : Suit_Id; Rank : Rank_Id) return Card_Handle;
   -- Allows any card to be constructed

   function Rank (Item : Card_Handle) return Rank_Id;
   -- Returns the rank of Item

   function Suit (Item : Card_Handle) return Suit_Id;
   -- Returns the suit of Item

   function Image (Item : Card_Handle) return String;
   -- Returns a 2-character string [subtype String (1 .. 2)] containing the image of Item
   -- Format is "RS"; R=Rank, S=Suit
   -- For Two .. Nine, R is the corresponding digit; for Ace and Ten .. King, R is the 1st character (A, T, etc)
   -- S is the 1st character (D, C, H, or S)
   -- Thus, Image (Make (Club, Seven) ) = "7C"
   --       Image (Make (Spade, Jack) ) = "JS"
private -- PragmARC.US_Card
   type Card_Handle is record
      Suit : Suit_Id := Suit_Id'First;
      Rank : Rank_Id := Rank_Id'First;
   end record;
end PragmARC.US_Card;
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