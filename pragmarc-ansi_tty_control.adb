-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Ansi_Tty_Control is
   function Position (Line : Positive := 1; Column : Positive := 1) return String is
      Prefix     : constant String    := Latin_1.Esc & '[';
      Separator  : constant Character := ';';
      Terminator : constant Character := 'H';

      Line_Image   : String := Positive'Image (Line);
      Column_Image : String := Positive'Image (Column);
   begin -- Position
      Zero_Fill_Line : for Index in Line_Image'Range loop
         exit Zero_Fill_Line when Line_Image (Index) /= ' ';

         Line_Image (Index) := '0';
      end loop Zero_Fill_Line;

      Zero_Fill_Column : for Index in Column_Image'Range loop
         exit Zero_Fill_Column when Column_Image (Index) /= ' ';

         Column_Image (Index) := '0';
      end loop Zero_Fill_Column;

      return Prefix & Line_Image & Separator & Column_Image & Terminator;
   end Position;
end PragmARC.Ansi_Tty_Control;
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
