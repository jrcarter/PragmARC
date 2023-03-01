-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Given "<", derives the other comparison operators
--
-- History:
-- 2023 Mar 01     J. Carter          V1.0--Initial version
--
generic -- PragmARC.Comparisons
   type T (<>) is limited private;

   with function "<" (Left : in T; Right : in T) return Boolean is <>;
package PragmARC.Comparisons with Pure is
   function ">"  (Left : in T; Right : in T) return Boolean is (Right < Left)                               with Inline;
   function ">=" (Left : in T; Right : in T) return Boolean is (not (Left < Right) )                        with Inline;
   function "<=" (Left : in T; Right : in T) return Boolean is (not (Right < Left) )                        with Inline;
   function "="  (Left : in T; Right : in T) return Boolean is (not (Right < Left) and not (Left < Right) ) with Inline;
end PragmARC.Comparisons;
