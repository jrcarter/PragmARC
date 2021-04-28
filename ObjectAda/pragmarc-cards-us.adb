-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Cards.US is
   function Image (Item : in Card_Info) return Image_String is
      Result : Image_String;
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
end PragmARC.Cards.US;
