-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Control strings for ANSI-standard terminals
-- These strings, when sent to an ANSI-standard terminal, have the stated effect
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Characters.Latin_1;

package PragmARC.Ansi_Tty_Control is
   use Ada.Characters;

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
