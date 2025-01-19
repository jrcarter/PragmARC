-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2025 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Unique Block Iteration (UBI) is a chaining mode that uses Threefish to build a compression function that maps an arbitrary
-- input size to a fixed output size.
--
-- Root of the UBI hierarchy
--
-- 2025 Feb 01     J. Carter     V1.0--Initial version

with PragmARC.Encryption.Threefish;

package PragmARC.UBI is
   subtype Byte_List is Encryption.Threefish.Byte_List; -- Subtypes for children
   use type Byte_List;
   subtype Couple is Encryption.Threefish.Couple;
   subtype Word is Encryption.Threefish.Word;
   use type Word;
end PragmARC.UBI;
