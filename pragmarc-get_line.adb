-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2005 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2005 Jul 01     J. Carter          V1.2--"or else" to avoid unnecessary End_Of_File
-- 2004 Dec 01     J. Carter          V1.1--added check for End_Of_File
-- 2000 May 01     J. Carter          V1.0--Initial release
--
function PragmARC.Get_Line (File : Text_IO.File_Type := Text_IO.Current_Input) return String is
   Line : String (1 .. 100);
   Last : Natural;
begin -- PragmARC.Get_Line
   Text_IO.Get_Line (File => File, Item => Line, Last => Last);

   if Last < Line'Last or else Text_IO.End_Of_File (File) then
      return Line (Line'First .. Last);
   else
      return Line & Get_Line (File);
   end if;
end PragmARC.Get_Line;
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