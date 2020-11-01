-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generic quick sort
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Jun 01     J. Carter          V1.2--Require Index'First = 1
-- 2019 Apr 01     J. Carter          V1.1--Require integer index
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
--------------------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V2.0--Package with sequential and parallel sorts
-- 2002 Oct 01     J. Carter          V1.1--Use mode out to allow scalars
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Sorting.Quick
   type Element  is private;
   type Index    is range <>; -- Lower bound of 1; see pragma Assert below
   type Sort_Set is array (Index range <>) of Element;

   with function "<" (Left : Element; Right : Element) return Boolean is <>;
package PragmARC.Sorting.Quick is
   pragma Assert (Index'First = 1);

   procedure Sort_Sequential (Set : in out Sort_Set) with
      Post => (for all I in Set'First .. Set'Last - 1 => not (Set (I + 1) < Set (I) ) );
   -- Sorts Set.
   --
   -- Time: O(N log N)

   procedure Sort_Parallel (Set : in out Sort_Set; Max_Tasks : in Natural := 1) with
      Post => (for all I in Set'First .. Set'Last - 1 => not (Set (I + 1) < Set (I) ) );
   -- Sorts Set using up to Max_Tasks additional tasks.
   --
   -- Time: O(N log N)
end PragmARC.Sorting.Quick;
