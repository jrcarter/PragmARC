-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Root of the Threefish cipher hierarchy
--
-- 2022 Aug 15     J. Carter     V1.3--Add "with Preelaborate"
-- 2022 Feb 01     J. Carter     V1.2--Reorganization for 512- and 1024-bit versions
-- 2021 May 01     J. Carter     V1.1--Adhere to coding standard
-- 2021 Feb 01     J. Carter     V1.0--Initial PragmARC version
--
with Interfaces;

package PragmARC.Encryption.Threefish with Preelaborate is
   subtype Word is Interfaces.Unsigned_64;

   type Word_List is array (Natural range <>) of Word;
   -- The Threefixh specification uses zeor-based indexing

   subtype Couple is Word_List (0 .. 1);

   subtype Byte is Interfaces.Unsigned_8;

   type Byte_List  is array (Positive range <>) of Byte;

   subtype Word_As_Bytes is Byte_List (1 ..  8); -- 1 => LSB, 8 => MSB

   function Word_From_Bytes (List : in Word_As_Bytes) return Word;
   function Bytes_From_Word (Value : in Word) return Word_As_Bytes;
   -- Endian-independent conversions
end PragmARC.Encryption.Threefish;
