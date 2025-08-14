-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Skein is a family of hash functions with three different internal state sizes: 256, 512, and 1024 bits
--
-- History:
-- 2025 Aug 15     J. Carter     V1.2--Common definition of a byte
-- 2025 Jul 01     J. Carter     V1.1--Use SPDX license format
-- 2025 Feb 01     J. Carter     V1.0--Initial version

with PragmARC.UBI;

package PragmARC.Skein is
   Bytes_For_256_Bits  : constant := 32; -- Number of output bytes for common hash lengths in bits
   Bytes_For_512_Bits  : constant := 2 * Bytes_For_256_Bits;
   Bytes_For_1024_Bits : constant := 2 * Bytes_For_512_Bits;

   type Block_Size_ID is (Size_256, Size_512, Size_1024);

   function Hash (Message : in Byte_List; Num_Bytes : in Positive; Block_Size : in Block_Size_ID) return Byte_List with
      Post => Hash'Result'Length = Num_Bytes;
private -- PragmARC.Skein
   subtype Word is UBI.Word; -- Subtypes for children
   use type Word;
   subtype Couple is UBI.Couple;
end PragmARC.Skein;
