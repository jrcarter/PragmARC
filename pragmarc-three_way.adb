-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
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
