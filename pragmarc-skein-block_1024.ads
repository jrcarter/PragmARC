-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Implementation of Skein-1024
--
-- History:
-- 2025 Jul 01     J. Carter     V1.1--Use SPDX license format
-- 2025 Feb 01     J. Carter     V1.0--Initial version

private package PragmARC.Skein.Block_1024 is
   function Hash (Message : in Byte_List; Num_Bytes : in Positive) return Byte_List;
end PragmARC.Skein.Block_1024;
