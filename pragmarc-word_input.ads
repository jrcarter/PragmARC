-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides the capability to get a word from a text file
-- A word is a sequence of non-whitespace characters separated by whitespace
-- Whitespace is one or more spaces, horizontal tabs, or line terminators
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Bounded;
with Ada.Text_Io;

use Ada;
generic -- PragmARC.Word_Input
   Max_Word : Positive := 40; -- Maximum # of input characters in a word
package PragmARC.Word_Input is
   package V_String is new Strings.Bounded.Generic_Bounded_Length (Max => Max_Word);

   subtype Word is V_String.Bounded_String;

   Word_Too_Long : exception; -- Raised when a Word in input is longer than Max_Word

   procedure Get (File : in Text_Io.File_Type; Value : out Word); -- raise Word_Too_Long, Text_Io.End_Error
   -- Obtains the next Word from File using Text_Io.Get
   -- Raises Word_Too_Long if the Word won't fit
   -- Raises Text_Io.End_Error if File is at the end of file
   -- Value is undefined if an exception is raised

   procedure Get (Value : out Word); -- raise Word_Too_Long, Text_Io.End_Error
   -- Obtains the next Word from Text_Io.Current_Input
end PragmARC.Word_Input;
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