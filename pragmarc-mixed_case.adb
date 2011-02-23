-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;

use Ada.Characters.Handling;
function PragmARC.Mixed_Case (S : String) return String is
   Result : String (S'range);
   Upper  : Boolean := True; -- True if next character should be upper case; True for 1st character
begin -- PragmARC.Mixed_Case
   All_Chars : for I in S'range loop
      if Upper then
         Result (I) := To_Upper (S (I) );
      else
         Result (I) := To_Lower (S (I) );
      end if;
      
      Upper := S (I) = '_' or S (I) = '.'; -- Character following an underline or dot should be upper case
   end loop All_Chars;
   
   return Result;
end PragmARC.Mixed_Case;
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
