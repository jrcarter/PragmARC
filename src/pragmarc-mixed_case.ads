-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Convert a string to mixed case
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2002 Jul 01     J. Carter          V1.1--Changed Pure to Preelaborate
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

function PragmARC.Mixed_Case (S : String) return String with Preelaborate;
-- Converts S to mixed case
-- (1st character and any character after an underline or dot in upper case,
-- other characters in lower case)
-- Function result has same bounds as S
