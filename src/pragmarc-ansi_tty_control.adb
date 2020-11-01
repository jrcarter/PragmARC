-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Fixed;

package body PragmARC.Ansi_Tty_Control is
   function Position (Line : Positive := 1; Column : Positive := 1) return String is
      Prefix     : constant String    := Latin_1.Esc & '[';
      Separator  : constant Character := ';';
      Terminator : constant Character := 'H';

      Line_Image   : constant String := Ada.Strings.Fixed.Trim (Integer'Image (Line),   Ada.Strings.Both);
      Column_Image : constant String := Ada.Strings.Fixed.Trim (Integer'Image (Column), Ada.Strings.Both);
   begin -- Position
      return Prefix & Line_Image & Separator & Column_Image & Terminator;
   end Position;
end PragmARC.Ansi_Tty_Control;
