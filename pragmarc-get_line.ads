-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- A function to obtain an entire line of text from an input file, & skip the line terminator
-- Modified from the algorithm in the article "Variable-Length String Input in Ada" by J. Carter in ADA LETTERS, 1989 May/Jun
--
-- History:
-- 2000 Dec 01     J. Carter          V1.1--Removed pragma Pure
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Text_Io;

use Ada;
function PragmARC.Get_Line (File : Text_Io.File_Type := Text_Io.Current_Input) return String;
-- Returns all characters remaining on the current line of File, & skips the line terminator
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