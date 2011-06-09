-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Determine a unit's full name
--
-- History:
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Fixed;
with Ada.Tags;
with PragmARC.Mixed_Case;

use Ada.Strings.Fixed;
use Ada.Tags;
generic -- PragmARC.Reflection
   -- null;
package PragmARC.Reflection is
   Unit_Name : constant String;
   -- The full name of the unit in which this package is instantiated
   -- (the null string if instantiated at the library level).
   -- Unit_Name is in strict mixed-case form [1st character and every character after
   -- a dot ('.') or underline ('_') is in upper case, and all other characters
   -- are in lower case].
private -- PragmARC.Reflection
   type T is abstract tagged null record;
   
   Name : constant String := Expanded_Name (T'Tag);
   Dot  : constant String := ".";
   
   Direction : constant Ada.Strings.Direction := Ada.Strings.Backward;
   
   Dot_Location : constant Natural := Index (Name (Name'First .. Index (Name, Dot, Direction) - 1), Dot, Direction);
   -- 1st Index finds dot before type name
   -- 2nd Index finds dot before instantiation name
   
   Unit_Name : constant String := Mixed_Case (Name (Name'First .. Dot_Location - 1) ); -- Trim off type name and instantiation name
end PragmARC.Reflection;
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