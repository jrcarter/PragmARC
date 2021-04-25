-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.0--Adhere to coding standard for Ada-12
-----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Latin_1;

package body PragmARC.Word_Input is
   use Ada.Characters;

   White : constant Character := ' ';

   function Get (File : in Text_IO.File_Type) return Character;
   -- Help function to return White for end of line and for horizontal tab

   function Get (File : in Text_IO.File_Type) return Character is
      Char : Character;
   begin -- Get
      if Text_IO.End_Of_Line (File) then -- Convert end of line to White
         Text_IO.Skip_Line (File);

         return White;
      else
         Text_IO.Get (File, Char);

         if Char = Latin_1.Ht then
            return White;
         end if;

         return Char;
      end if;
   end Get;

   procedure Get (File : in Text_IO.File_Type; Value : out Word) is
      Char : Character;
   begin -- Get
      Value := V_String.Null_Bounded_String;

      Skip_White : loop
         Char := Get (File);

         exit Skip_White when Char /= White;
      end loop Skip_White;

      Fill_Word : loop
         exit Fill_Word when Char = White;

         V_String.Append (Source => Value, New_Item => Char);
         Char := Get (File);
      end loop Fill_Word;
   exception -- Get
   when Strings.Length_Error =>
      raise Too_Short;
   end Get;

   procedure Get (Value : out Word) is
      -- Empty
   begin -- Get
      Get (File => Text_IO.Current_Input, Value => Value);
   end Get;
end PragmARC.Word_Input;
