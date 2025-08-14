-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Root of the Threefish cipher hierarchy
--
-- History:
-- 2025 Aug 15     J. Carter     V1.5--Common definition of a byte
-- 2025 Jul 01     J. Carter     V1.4--Use SPDX license format
-- 2022 Aug 15     J. Carter     V1.3--Make Pure
-- 2022 Feb 01     J. Carter     V1.2--Reorganization for 512- and 1024-bit versions
-- 2021 May 01     J. Carter     V1.1--Adhere to coding standard
-- 2021 Feb 01     J. Carter     V1.0--Initial PragmARC version
--
with Interfaces;

package PragmARC.Encryption.Threefish with Pure is
   subtype Word is Interfaces.Unsigned_64;

   type Word_List is array (Natural range <>) of Word;
   -- The Threefish specification uses zero-based indexing

   subtype Couple is Word_List (0 .. 1);

   subtype Word_As_Bytes is Byte_List (1 .. 8); -- 1 => LSB, 8 => MSB

   function Word_From_Bytes (List : in Word_As_Bytes) return Word;
   function Bytes_From_Word (Value : in Word) return Word_As_Bytes;
   -- Endian-independent conversions
end PragmARC.Encryption.Threefish;
