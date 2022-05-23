-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Luhn checksum generation
--
-- History:
-- 2022 Jun 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Luhn_Generation is
   subtype Digit is Integer range 0 .. 9;

   function Checksum (Input : in String) return Digit with
      Pre => Input'Length > 0 and (for some C of Input => C in '0' .. '9');
   -- Returns the Luhn checksum for the digits in Input
   -- Non-digit characters in Input are ignored
end PragmARC.Luhn_Generation;
