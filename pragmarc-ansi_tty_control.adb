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
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Fixed;

package body PragmARC.Ansi_Tty_Control is
   function Position (Line : Positive := 1; Column : Positive := 1) return String is
      Prefix     : constant String    := Ada.Characters.Latin_1.Esc & '[';
      Separator  : constant Character := ';';
      Terminator : constant Character := 'H';

      Line_Image   : constant String := Ada.Strings.Fixed.Trim (Integer'Image (Line),   Ada.Strings.Both);
      Column_Image : constant String := Ada.Strings.Fixed.Trim (Integer'Image (Column), Ada.Strings.Both);
   begin -- Position
      return Prefix & Line_Image & Separator & Column_Image & Terminator;
   end Position;
end PragmARC.Ansi_Tty_Control;
