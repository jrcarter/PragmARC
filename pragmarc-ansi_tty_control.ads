-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Control strings for ANSI-standard terminals
-- These strings, when sent to an ANSI-standard terminal, have the stated effect
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Latin_1;

use Ada.Characters;
package PragmARC.Ansi_Tty_Control is
   pragma Pure;

   -- Screen and line clearing:
   Clear_Screen : constant String := Latin_1.Esc & "[;H" & Latin_1.Esc & "[2J";
   -- Clears the screen & positions cursor at upper left corner

   Clear_End_Of_Line : constant String := Latin_1.Esc & "[0K";
   -- Clears the line containing the cursor from the cursor to the end of the line

   -- Cursor moving:
   Cursor_Up : constant String := Latin_1.Esc & "1A";
   -- Moves the cursor up one line
   -- No effect if on top line

   Cursor_Down : constant String := Latin_1.Esc & "1B";
   -- Moves the cursor down one line
   -- No effect if on bottom line

   Cursor_Right : constant String := Latin_1.Esc & "1C";
   -- Moves the cursor right one column
   -- No effect if at rightmost column

   Cursor_Left : constant String := Latin_1.Esc & "1D";
   -- Moves the cursor left one column
   -- No effect if at first column

   function Position (Line : Positive := 1; Column : Positive := 1) return String;
   -- Positions the cursor to given line and column
   -- Line 1 is the top line and column 1 is the left column

   -- Cursor report:
   Report_Cursor : constant String := Latin_1.Esc & "[6n";
   -- Adds a string of the form Latin_1.Esc & "[l;cR" to standard input
   -- l is replaced by the String representation of the row number (top line is line 1)
   -- c is replaced by the String representation of the column number (left column is column 1)

   -- Cursor saving and restoring:
   Save_Cursor : constant String := Latin_1.Esc & "[s";
   -- Saves the current cursor position; may be restored by Restore_Cursor

   Restore_Cursor : constant String := Latin_1.Esc & "[u";
   -- Moves the cursor to the position saved by Save_Cursor

   -- Text modes:
   Normal_Mode : constant String := Latin_1.Esc & "[0m";
   -- Starts normal mode (default)

   Bold_Mode : constant String := Latin_1.Esc & "[1m";
   -- Starts bold mode

   Underline_Mode : constant String := Latin_1.Esc & "[4m";
   -- Starts underline mode

   Blinking_Mode : constant String := Latin_1.Esc & "[5m";
   -- Starts blinking mode

   Reverse_Video : constant String := Latin_1.Esc & "[7m";
   -- Starts reverse video mode

   -- All of these non-normal modes are terminated by Normal_Mode
end PragmARC.Ansi_Tty_Control;
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