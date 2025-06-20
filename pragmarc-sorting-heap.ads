-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Generic heap sort
--
-- History:
-- 2025 Jul 01     J. Carter          V2.2--Use SPDX license format
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.1--Sequences indexed by integers
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Sorting.Heap
   type Element  is private;
   type Index    is range <>; -- Lower bound should be 1
   type Sort_Set is array (Index range <>) of Element;

   with function "<" (Left : in Element; Right : in Element) return Boolean is <>;
procedure PragmARC.Sorting.Heap (Set : in out Sort_Set) with Pure,
   Post => (for all I in Set'First .. Set'Last - 1 => not (Set (I + 1) < Set (I) ) );
-- Input:  Set to sort
-- Output: Sorted set
--
-- Time: O(N log N)
