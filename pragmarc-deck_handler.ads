-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- A simplified cross between a list and a queue to be used as a deck of cards in card games
--
-- History:
-- 2002 Feb 01     J. Carter          V1.1--Removed pragma Pure
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Deck_Handler
   type Card is private; -- A deck of cards
package PragmARC.Deck_Handler is
   type Handle (Max_Cards : Positive) is limited private;
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

   procedure Add (Item : in Card; To : in out Handle); -- raise Full
   -- Adds Item to the bottom of To
   -- Raises Full if To is full
   -- To is unchanged if Full is raised
   --
   -- Time: O(1)
   --
   -- Precondition:  not Is_Full (To)     raise Full if violated
   --
   -- Postcondition: not Is_Empty (To)

   procedure Deal (From : in out Handle; To : out Card); -- raise Empty
   -- Removes the top card from From and assigns it to To
   -- Identical to Remove (From, 1, To);
   -- Raises Empty if From is empty
   -- From is unchanged if Empty is raised
   -- To is undefined if Empty is raised
   --
   -- Time: O(N)
   --
   -- Precondition:  not Is_Empty (From)     raise Empty if violated
   --
   -- Postcondition: not Is_Full (From)

   procedure Remove (From : in out Handle; Position : in Positive; To : out Card); -- raise Empty
   -- Allows you to "deal" from any position in the Handle
   -- Position 1 is the top of the deck
   -- Position Size (From) is the bottom of the deck
   -- Raises Empty if Position not in 1 .. Size (From)
   -- From is unchanged if Empty is raised
   -- To is undefined if Empty is raised
   --
   -- Time: O(N)
   --
   -- Precondition:  Position in 1 .. Size (From)     raise Empty if violated
   --
   -- Postcondition: not Is_Full (From)

   function Value (From : Handle; Position : Positive) return Card; -- raise Empty
   -- Provides the value of the card at Position in From, without removing it
   -- Position 1 is the top of the deck
   -- Position Size (From) is the bottom of the deck
   -- Raises Empty if Position not in 1 .. Size (From)
   --
   -- Time: O(1)
   --
   -- Precondition:  Position in 1 .. Size (From)     raise Empty if violated

   procedure Assign (To : in out Handle; From : in Handle); -- raise Full
   -- Gives To the same value as From
   -- Raises Full if From won't fit in To
   -- To is unchanged if Full is raised
   --
   -- Time: O(N)
   --
   -- Precondition:  To.Max_Cards >= From.Max_Cards     raise Full if violated
   --
   -- Postcondition: To = From

   function "=" (Left : Handle; Right : Handle) return Boolean;
   -- Returns True if Left and Right contain the same deck; False otherwise
   --
   -- Time: O(N)
private -- PragmARC.Deck_Handler
   type Deck_Set is array (Positive range <>) of Card;

   type Handle (Max_Cards : Positive) is record
      Count : Natural := 0;
      Value : Deck_Set (1 .. Max_Cards);
   end record;
end PragmARC.Deck_Handler;
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