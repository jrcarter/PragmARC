-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Convert a string to mixed case
--
-- History:
-- 2025 Jul 01     J. Carter          V2.2--Use SPDX license format
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2002 Jul 01     J. Carter          V1.1--Changed Pure to Preelaborate
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

function PragmARC.Mixed_Case (S : in String) return String with Preelaborate;
-- Converts S to mixed case
-- (1st character and any character after an underline or dot in upper case,
-- other characters in lower case)
-- Function result has same bounds as S
