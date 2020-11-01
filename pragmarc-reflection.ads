-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Determine a unit's full name
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 Dec 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Strings.Fixed;
with Ada.Tags;
with PragmARC.Mixed_Case;

generic -- PragmARC.Reflection
   -- Empty
package PragmARC.Reflection is
   Unit_Name : constant String;
   -- The full name of the unit in which this package is instantiated
   -- (if instantiated at the library level, the parent name if it's a child unit and the null string if it's not).
   -- Unit_Name is in strict mixed-case form [1st character and every character after
   -- a dot ('.') or underline ('_') is in upper case, and all other characters
   -- are in lower case].
private -- PragmARC.Reflection
   use Ada.Strings.Fixed;
   use Ada.Tags;

   type T is abstract tagged null record;

   Name : constant String := Expanded_Name (T'Tag);
   Dot  : constant String := ".";

   Direction : constant Ada.Strings.Direction := Ada.Strings.Backward;

   Dot_Location : constant Natural := Index (Name (Name'First .. Index (Name, Dot, Direction) - 1), Dot, Direction);
   -- 1st Index finds dot before type name
   -- 2nd Index finds dot before instantiation name

   Unit_Name : constant String := Mixed_Case (Name (Name'First .. Dot_Location - 1) );
   -- Trim off type name and instantiation name
end PragmARC.Reflection;
