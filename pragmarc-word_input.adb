-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Latin_1;

use Ada.Characters;
package body PragmARC.Word_Input is
   use type V_String.Bounded_String;

   White : constant Character := ' ';

   -- Help function to return White for end of line and for horizontal tab
   function Get (File : Text_Io.File_Type) return Character is
      Char : Character;
   begin -- Get
      if Text_Io.End_Of_Line (File) then -- Convert end of line to White
         Text_Io.Skip_Line (File);

         return White;
      else
         Text_Io.Get (File, Char);

         if Char = Latin_1.Ht then
            return White;
         else
            return Char;
         end if;
      end if;
   end Get;

   procedure Get (File : in Text_Io.File_Type; Value : out Word) is
      Char : Character;
   begin -- Get
      Value := V_String.Null_Bounded_String;

      Skip_White : loop
         Char := Get (File);

         exit Skip_White when Char /= White;
      end loop Skip_White;

      Fill_Word : loop
         exit Fill_Word when Char = White;

         Value := Value & Char;
         Char := Get (File);
      end loop Fill_Word;
   exception -- Get
   when Strings.Length_Error =>
      raise Word_Too_Long;
   end Get;

   procedure Get (Value : out Word) is
      -- null;
   begin -- Get
      Get (File => Text_Io.Current_Input, Value => Value);
   end Get;
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