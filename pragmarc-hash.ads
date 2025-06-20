-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- A good quality hash function that gives the same results for the same definition of Hash_Type regardless of the platform
--
-- History:
-- 2025 Jul 01     J. Carter          V1.2--Use SPDX license format
-- 2022 May 15     J. Carter          V1.1--Activated checks
-- 2022 Apr 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers;

function PragmARC.Hash (Key : in String) return Ada.Containers.Hash_Type;
