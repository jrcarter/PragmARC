-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Pascal-like set type and operations
--
-- History:
-- 2002 Sep 01     J. Carter          V1.1--Added renamed functions
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Set_Discrete
   type Element is (<>);
package PragmARC.Set_Discrete is
   pragma Pure;

   type Set is private; -- Initial value: empty

   Empty : constant Set; -- No members
   Full  : constant Set; -- Every possible member

   function "+" (Left : Set; Right : Set) return Set; -- Union
   function Union (Left : Set; Right : Set) return Set renames "+";

   function "+" (Left : Set;     Right : Element) return Set; -- These two perform the union of the Set and a Set consisting of
   function "+" (Left : Element; Right : Set)     return Set; -- the Element
   function Union (Left : Set;     Right : Element) return Set renames "+";
   function Union (Left : Element; Right : Set)     return Set renames "+";

   function "*" (Left : Set; Right : Set) return Set; -- Intersection
   function Intersection (Left : Set; Right : Set) return Set renames "*";

   function "-" (Left : Set; Right : Set) return Set; -- Difference
   function Difference (Left : Set; Right : Set) return Set renames "-";

   function "-" (Left : Set; Right : Element) return Set; -- Performs the difference of Left and a Set consisting of Right
   function Difference (Left : Set; Right : Element) return Set renames "-";

   function "/" (Left : Set; Right : Set) return Set; -- Symmetric difference
   function Symmetric_Differece (Left : Set; Right : Set) return Set renames "/";

   function "<=" (Left : Set; Right : Set) return Boolean; -- Subset
   function Subset (Left : Set; Right : Set) return Boolean renames "<=";

   function "<" (Left : Set; Right : Set) return Boolean; -- Proper subset
   function Proper_Subset (Left : Set; Right : Set) return Boolean renames "<";

   function ">=" (Left : Set; Right : Set) return Boolean; -- Superset
   function Superset (Left : Set; Right : Set) return Boolean renames ">=";

   function ">" (Left : Set; Right : Set) return Boolean; -- Proper superset
   function Proper_Superset (Left : Set; Right : Set) return Boolean renames ">";

   function Member (Item : Element; Group : Set) return Boolean; -- Membership

   type Member_List is array (Positive range <>) of Element;

   function Make_Set (List : Member_List) return Set;
   -- Allows a sort of Set literal:
   -- My_Set := Make_Set ( (An_Element, Another_Element, Yet_A_3rd_Element) );

   function Size (Group : Set) return Natural; -- Number of members
private -- PragmARC.Set_Discrete
   type Set_List is array (Element) of Boolean;
   for Set_List'Component_Size use 1;
   pragma Pack (Set_List);

   type Set is record
      Value : Set_List := Set_List'(others => False);
   end record;

   Empty : constant Set := Set'(Value => Set_List'(others => False) );
   Full  : constant Set := Set'(Value => Set_List'(others => True ) );
end PragmARC.Set_Discrete;
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