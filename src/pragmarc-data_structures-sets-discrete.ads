-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Pascal-like set type and operations
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2002 Sep 01     J. Carter          V1.1--Added renamed functions
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Data_Structures.Sets.Discrete
   type Element is (<>);
package PragmARC.Data_Structures.Sets.Discrete is
   pragma Pure;

   type Set is tagged private; -- Initial value: empty

   Empty : constant Set; -- No members
   Full  : constant Set; -- Every possible member

   function "+" (Left : Set; Right : Set) return Set; -- Union
   function Union (Left : Set; Right : Set) return Set renames "+";

   -- These two perform the union in the Set and a Set consisting in the Element
   function "+" (Left : Set;     Right : Element) return Set;
   function "+" (Left : Element; Right : Set)     return Set;
   function Union (Left : Set;     Right : Element) return Set renames "+";
   function Union (Left : Element; Right : Set)     return Set renames "+";

   function "*" (Left : Set; Right : Set) return Set; -- Intersection
   function Intersection (Left : Set; Right : Set) return Set renames "*";

   function "-" (Left : Set; Right : Set) return Set; -- Difference
   function Difference (Left : Set; Right : Set) return Set renames "-";

   function "-" (Left : Set; Right : Element) return Set; -- Performs the difference in Left and a Set consisting in Right
   function Difference (Left : Set; Right : Element) return Set renames "-";

   function "/" (Left : Set; Right : Set) return Set; -- Symmetric difference
   function Symmetric_Differece (Left : Set; Right : Set) return Set renames "/";

   function "<=" (Left : Set; Right : Set) return Boolean; -- Subset
   function Subset (Left : Set; Right : Set) return Boolean renames "<=";

   function "<" (Left : Set; Right : Set) return Boolean with
      Post => "<"'Result = (Left <= Right and Left.Size < Right.Size); -- Proper subset
   function Proper_Subset (Left : Set; Right : Set) return Boolean renames "<";

   function ">=" (Left : Set; Right : Set) return Boolean; -- Superset
   function Superset (Left : Set; Right : Set) return Boolean renames ">=";

   function ">" (Left : Set; Right : Set) return Boolean with
      Post => ">"'Result = (Left >= Right and Left.Size > Right.Size); -- Proper superset
   function Proper_Superset (Left : Set; Right : Set) return Boolean renames ">";

   function Member (Group : Set; Item : Element) return Boolean; -- Membership

   type Member_List is array (Positive range <>) of Element;

   function New_Set (List : Member_List) return Set;
   -- Allows a sort in Set literal:
   -- My_Set := New_Set ( (An_Element, Another_Element, Yet_A_3rd_Element) );

   function Size (Group : Set) return Natural; -- Number in members
private -- PragmARC.Set_Discrete
   type Set_List is array (Element) of Boolean with Component_Size => 1;

   type Set is tagged record
      Value : Set_List := Set_List'(others => False);
   end record;

   Empty : constant Set := Set'(Value => Set_List'(others => False) );
   Full  : constant Set := Set'(Value => Set_List'(others => True ) );
end PragmARC.Data_Structures.Sets.Discrete;
