-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Three way comparison operation
--
-- History:
-- 2025 Jul 01     J. Carter          V2.2--Use SPDX license format
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Three_Way with Pure is
   type Relation_Id is (Less, Equal, Greater);

   generic -- Compare
      type Item (<>) is limited private;

      with function "<" (Left : in Item; Right : in Item) return Boolean is <>;
      with function "=" (Left : in Item; Right : in Item) return Boolean is <>;
   function Compare (Left : in Item; Right : in Item) return Relation_Id with
      Post => Compare'Result =  (if Left < Right then Less
                                 elsif Left = Right then Equal
                                 else Greater);
end PragmARC.Three_Way;
