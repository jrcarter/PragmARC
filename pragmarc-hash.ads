-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A good quality hash function that gives the same results for the same definition of Hash_Type regardless of the platform
--
-- History:
-- 2022 Apr 01     J. Carter          V1.0--Initial version
--
with Ada.Containers;

function PragmARC.Hash (Key : in String) return Ada.Containers.Hash_Type;
