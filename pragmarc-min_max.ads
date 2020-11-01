-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Minimum and maximum functions for non-scalar values for which "<" is meaningful
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Min_Max
   type Element (<>) is limited private;

   with function "=" (Left : Element; Right : Element) return Boolean is <>;
   with function "<" (Left : Element; Right : Element) return Boolean is <>;
package PragmARC.Min_Max with Pure is
   function Min (Left : Element; Right : Element) return Element with
      Post => Min'Result = (if Left < Right then Left else Right);
   function Max (Left : Element; Right : Element) return Element with
      Post => Max'Result = (if Left < Right then Right else Left);
end PragmARC.Min_Max;
