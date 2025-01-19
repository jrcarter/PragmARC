-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2025 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of Skein-256
--
-- 2025 Feb 01     J. Carter     V1.0--Initial version

private package PragmARC.Skein.Block_256 is
   function Hash (Message : in Byte_List; Num_Bytes : in Positive) return Byte_List;
end PragmARC.Skein.Block_256;
