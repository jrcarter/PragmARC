-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2025 Jul 01     J. Carter          V2.2--Use SPDX license format
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;

function PragmARC.Mixed_Case (S : in String) return String is
   Result : String (S'range);
   Upper  : Boolean := True; -- True if next character should be upper case; True for 1st character
begin -- PragmARC.Mixed_Case
   All_Chars : for I in S'range loop
      if Upper then
         Result (I) := Ada.Characters.Handling.To_Upper (S (I) );
      else
         Result (I) := Ada.Characters.Handling.To_Lower (S (I) );
      end if;

      Upper := S (I) in '_' | '.'; -- Character following an underline or dot should be upper case
   end loop All_Chars;

   return Result;
end PragmARC.Mixed_Case;
