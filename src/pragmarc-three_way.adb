-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Three_Way is
   function Compare  (Left : Item; Right : Item) return Relation_Id is
      -- Empty
   begin
      if Left < Right then
         return Less;
      elsif Left = Right then
         return Equal;
      else
         return Greater;
      end if;
   end Compare;
end PragmARC.Three_Way;
