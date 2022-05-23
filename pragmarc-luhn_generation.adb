-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2022 Jun 01     J. Carter          V1.0--Initial version
--
package body PragmARC.Luhn_Generation is
   function Checksum (Input : in String) return Digit is
      subtype Digit_Char is Character range '0' .. '9';

      function Reversed (Value : String) return String;
      -- Reverses Value.

      function Squeezed (Value : String) return String;
      -- Keeps the digits of Value and discards any other characters.

      function D2N (D : Digit_Char) return Natural is (Character'Pos (D) - Character'Pos ('0') );

      function Reversed (Value : String) return String is
         Result : String (Value'Range);
         Last   : Natural := Value'Last;
      begin -- Reversed
         if Value = "" then
            return "";
         end if;

         Swap : for First in Value'First .. Value'First + (Value'Length - 1) / 2 loop
            Result (First) := Value (Last);
            Result (Last)  := Value (First);
            Last := Last - 1;
         end loop Swap;

         return Result;
      end Reversed;

      function Squeezed (Value : String) return String is
         Result : String (1 .. Value'Length);
         Last   : Natural := 0;
      begin -- Squeezed
         All_Chars : for I in Value'Range loop
            if Value (I) in Digit_Char then
               Last := Last + 1;
               Result (Last) := Value (I);
            end if;
         end loop All_Chars;

         return Result (1 .. Last);
      end Squeezed;

      Value : constant String := Squeezed (Reversed (Input) );

      Sum : Natural := 0;
      D   : Natural;
   begin -- Checksum
      All_Digits : for I in Value'Range loop
         D := D2N (Value (I) );

         if I rem 2 = 1 then
            D := 2 * D;

            if D > 9 then
               D := D - 9;
            end if;
         end if;

         Sum := Sum + D;
      end loop All_Digits;

      return (9 * Sum) rem 10;
   end Checksum;
end PragmARC.Luhn_Generation;
