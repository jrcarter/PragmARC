-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Instantiation of PragmARC.Regular_Expression_Matcher for strings
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Regular_Expression_Matcher;
package PragmARC.Character_Regular_Expression_Matcher is
new PragmARC.Regular_Expression_Matcher (Item             => Character,
                                         Index            => Positive,
                                         Item_Set         => String,
                                         Any_Item         => '?',
                                         Escape_Item      => '&',
                                         Not_Item         => '~',
                                         Closure_Item     => '*',
                                         Start_Class_Item => '[',
                                         Stop_Class_Item  => ']'
                                        )
;
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