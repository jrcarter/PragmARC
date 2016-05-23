-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.US_Deck is
   procedure Standard_Deck (Item : in out Deck_52) is
      -- Empty
   begin -- Standard_Deck
      Deck.Make_Empty (Item => Item);

      All_Suits : for Suit in US_Card.Suit_Id loop
         All_Ranks : for Rank in US_Card.Rank_Id loop
            Deck.Add (Item => US_Card.Make (Suit, Rank), To => Item);
         end loop All_Ranks;
      end loop All_Suits;
   end Standard_Deck;
end PragmARC.US_Deck;
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
