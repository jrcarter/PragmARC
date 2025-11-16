-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Luhn checksum generation
--
-- History:
-- 2025 Nov 15     J. Carter          V1.2--Version that returns a Character
-- 2025 Jul 01     J. Carter          V1.1--Use SPDX license format
-- 2022 Jun 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Luhn_Generation is
   subtype Digit is Integer range 0 .. 9;
   subtype Digit_Char is Character range '0' .. '9';

   function Checksum (Input : in String) return Digit with
      Pre => Input'Length > 0 and (for some C of Input => C in Digit_Char);
   -- Returns the Luhn checksum for the digits in Input
   -- Non-digit characters in Input are ignored

   function Checksum (Input : in String) return Digit_Char is
      (Character'Val (Character'Pos (Digit_Char'First) + Checksum (Input) ) );
end PragmARC.Luhn_Generation;
