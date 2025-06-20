-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Generic radix sort
--
-- History:
-- 2025 Jul 01     J. Carter          V2.1--Use SPDX license format
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.1--Sequences indexed by integers
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Sorting.Radix
   type Element  is mod <>;
   type Index    is range <>; -- Lower bound should be 1
   type Sort_Set is array (Index range <>) of Element;
procedure PragmARC.Sorting.Radix (Set : in out Sort_Set) with Pure,
      Post => (for all I in Set'First .. Set'Last - 1 => not (Set (I + 1) < Set (I) ) );
-- Input:  Set to sort
-- Output: Sorted set
--
-- Time: O(N)
