-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Unique Block Iteration (UBI) is a chaining mode that uses Threefish to build a compression function that maps an arbitrary
-- input size to a fixed output size.
--
-- Root of the UBI hierarchy
--
-- History:
-- 2025 Jul 01     J. Carter     V1.1--Use SPDX license format
-- 2025 Feb 01     J. Carter     V1.0--Initial version

with PragmARC.Encryption.Threefish;

package PragmARC.UBI is
   subtype Byte_List is Encryption.Threefish.Byte_List; -- Subtypes for children
   use type Byte_List;
   subtype Couple is Encryption.Threefish.Couple;
   subtype Word is Encryption.Threefish.Word;
   use type Word;
end PragmARC.UBI;
