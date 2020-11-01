-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides the capability to get a word from a text file
-- A word is a sequence of non-whitespace characters separated by whitespace
-- Whitespace is one or more spaces, horizontal tabs, or line terminators
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Strings.Bounded;
with Ada.Text_IO;

generic -- PragmARC.Word_Input
   Max_Word : Positive := 40; -- Maximum # of input characters in a word
package PragmARC.Word_Input is
   use Ada;

   package V_String is new Strings.Bounded.Generic_Bounded_Length (Max => Max_Word);

   subtype Word is V_String.Bounded_String;

   procedure Get (File : in Text_IO.File_Type; Value : out Word) with
      Pre => Ada.Text_IO.Is_Open (File) and Ada.Text_IO.Mode (File) in Ada.Text_IO.In_File; -- raise Too_Short, Text_IO.End_Error
   -- Obtains the next Word from File using Text_IO.Get
   -- Raises Too_Short if the Word won't fit
   -- Raises Text_IO.End_Error if File is at the end of file
   -- Value is undefined if an exception is raised

   procedure Get (Value : out Word); -- raise Too_Short, Text_IO.End_Error
   -- Obtains the next Word from Text_IO.Current_Input
end PragmARC.Word_Input;
