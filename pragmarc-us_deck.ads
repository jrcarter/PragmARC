-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- An instantiation of PragmARC.Deck_Handler for the cards defined in PragmARC.US_Card
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Deck_Handler;
with PragmARC.US_Card;
package PragmARC.US_Deck is
   package Deck is new Deck_Handler (Card => US_Card.Card_Handle);

   subtype Deck_52 is Deck.Handle (Max_Cards => 52);

   procedure Standard_Deck (Item : in out Deck_52); -- Converts Deck into the standard US 52-card deck in suit & rank order
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