-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;

function PragmARC.Mixed_Case (S : String) return String is
   use Ada.Characters.Handling;

   Result : String (S'range);
   Upper  : Boolean := True; -- True if next character should be upper case; True for 1st character
begin -- PragmARC.Mixed_Case
   All_Chars : for I in S'range loop
      if Upper then
         Result (I) := To_Upper (S (I) );
      else
         Result (I) := To_Lower (S (I) );
      end if;

      Upper := S (I) in '_' | '.'; -- Character following an underline or dot should be upper case
   end loop All_Chars;

   return Result;
end PragmARC.Mixed_Case;
