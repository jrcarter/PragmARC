-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generic insertion sort
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.1--Sequences indexed by integers
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Sorting.Insertion
   type Element  is private;
   type Index    is range <>; -- Lower bound should be 1
   type Sort_Set is array (Index range <>) of Element;

   with function "<" (Left : Element; Right : Element) return Boolean is <>;
procedure PragmARC.Sorting.Insertion (Set : in out Sort_Set); -- with Pure,
--  Post => (for all I in Set'First .. Set'Last - 1 => not (Set (I + 1) < Set (I) ) );
-- Input:  Set to sort
-- Output: Sorted set
--
-- Time: O(N ** 2)
