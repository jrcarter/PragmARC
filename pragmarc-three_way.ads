-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Three way comparison operation
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package PragmARC.Three_Way is
   pragma Pure;

   type Relation_Id is (Less, Equal, Greater);

   generic -- Compare
      type Item (<>) is limited private;

      with function "<" (Left : Item; Right : Item) return Boolean is <>;
      with function "=" (Left : Item; Right : Item) return Boolean is <>;
   function Compare (Left : Item; Right : Item) return Relation_Id;
   -- Returns Less if Left < Right; Equal if Left = Right; Greater otherwise
end PragmARC.Three_Way;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.