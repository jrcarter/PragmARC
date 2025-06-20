-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Given "<", derives the other comparison operators
--
-- History:
-- 2025 Jul 01     J. Carter          V1.1--Use SPDX license format
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
