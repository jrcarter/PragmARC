-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.US_Card is
   function Make (Suit : Suit_Id; Rank : Rank_Id) return Card_Handle is
      -- null;
   begin -- Make
      return Card_Handle'(Suit => Suit, Rank => Rank);
   end Make;

   function Rank (Item : Card_Handle) return Rank_Id is
      -- null;
   begin -- Rank
      return Item.Rank;
   end Rank;

   function Suit (Item : Card_Handle) return Suit_Id is
      -- null;
   begin -- Suit
      return Item.Suit;
   end Suit;

   function Image (Item : Card_Handle) return String is
      Result : String (1 .. 2);
   begin -- Image
      case Item.Rank is
      when Ace =>
         Result (1) := 'A';
      when Two =>
         Result (1) := '2';
      when Three =>
         Result (1) := '3';
      when Four =>
         Result (1) := '4';
      when Five =>
         Result (1) := '5';
      when Six =>
         Result (1) := '6';
      when Seven =>
         Result (1) := '7';
      when Eight =>
         Result (1) := '8';
      when Nine =>
         Result (1) := '9';
      when Ten =>
         Result (1) := 'T';
      when Jack =>
         Result (1) := 'J';
      when Queen =>
         Result (1) := 'Q';
      when King =>
         Result (1) := 'K';
      end case;

      case Item.Suit is
      when Diamond =>
         Result (2) := 'D';
      when Club =>
         Result (2) := 'C';
      when Heart =>
         Result (2) := 'H';
      when Spade =>
         Result (2) := 'S';
      end case;

      return Result;
   end Image;
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