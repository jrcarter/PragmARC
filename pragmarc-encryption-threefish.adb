-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2025 Jul 01     J. Carter     V1.3--Use SPDX license format
-- 2022 Feb 01     J. Carter     V1.2--Reorganization for 512- and 1024-bit versions
-- 2021 May 01     J. Carter     V1.1--Adhere to coding standard
-- 2021 Feb 01     J. Carter     V1.0--Initial PragmARC version
--
with Ada.Unchecked_Conversion;
with System;

package body PragmARC.Encryption.Threefish is
   use type System.Bit_Order;

   procedure Reverse_Bytes (List : in out Word_As_Bytes);
   -- Reverses the bytes of List

   function Word_From_Bytes (List : in Word_As_Bytes) return Word is
      function To_Word is new Ada.Unchecked_Conversion (Source => Word_As_Bytes, Target => Word);

      Local : Word_As_Bytes := List;
   begin -- Word_From_Bytes
      if System.Default_Bit_Order = System.High_Order_First then
         Reverse_Bytes (List => Local);
      end if;

      return To_Word (Local);
   end Word_From_Bytes;

   function Bytes_From_Word (Value : in Word) return Word_As_Bytes is
      function To_Bytes is new Ada.Unchecked_Conversion (Source => Word, Target => Word_As_Bytes);

      Result : Word_As_Bytes := To_Bytes (Value);
   begin -- Bytes_From_Word
      if System.Default_Bit_Order = System.High_Order_First then
         Reverse_Bytes (List => Result);
      end if;

      return Result;
   end Bytes_From_Word;

   procedure Reverse_Bytes (List : in out Word_As_Bytes) is
      procedure Swap (Left : in out Byte; Right : in out Byte);
      -- Swaps Left and Right

      procedure Swap (Left : in out Byte; Right : in out Byte) is
         Temp : constant Byte := Left;
      begin -- Swap
         Left := Right;
         Right := Temp;
      end Swap;

      Last : Natural := List'Last;
   begin -- Reverse_Bytes
      Swap_All : for I in List'First .. Last / 2 loop
         Swap (Left => List (I), Right => List (Last) );
         Last := Last - 1;
      end loop Swap_All;
   end Reverse_Bytes;
end PragmARC.Encryption.Threefish;
