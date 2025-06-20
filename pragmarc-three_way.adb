-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2025 Jul 01     J. Carter          V2.3--Use SPDX license format
-- 2021 May 01     J. Carter          V2.2--Adhere to coding standard
-- 2020 Dec 01     J. Carter          V2.1--Expression function
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Three_Way is
   function Compare  (Left : in Item; Right : in Item) return Relation_Id is
      (if Left < Right then
         Less
      elsif Left = Right then
         Equal
      else
         Greater);
end PragmARC.Three_Way;
