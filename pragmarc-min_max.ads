-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Minimum and maximum functions for non-scalar values for which "<" is meaningful
--
-- History:
-- 2021 Feb 01     J. Carter          V2.2--Expression functions illegal for limited type
-- 2020 Dec 01     J. Carter          V2.1--Expression functions eliminate body; removed "="
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Min_Max
   type Element (<>) is private;

   with function "<" (Left : Element; Right : Element) return Boolean is <>;
package PragmARC.Min_Max with Pure is
   function Min (Left : Element; Right : Element) return Element is
      (if Left < Right then Left else Right);
   function Max (Left : Element; Right : Element) return Element is
      (if Left < Right then Right else Left);
end PragmARC.Min_Max;
