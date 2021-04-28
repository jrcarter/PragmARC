-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Pascal-like set type and operations
--
-- History:
-- 2021 May 01     J. Carter          V2.3--Adhere to coding standard
-- 2021 Jan 01     J. Carter          V2.2--Corrected comments
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
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
package PragmARC.Data_Structures.Sets.Discrete with Pure is
   type Set is tagged private; -- Initial value: empty

   Empty : constant Set; -- No members
   Full  : constant Set; -- Every possible member

   function "+" (Left : in Set; Right : in Set) return Set; -- Union
   function Union (Left : in Set; Right : in Set) return Set renames "+";

   -- These two perform the union of the Set and a Set consisting of the Element
   function "+" (Left : in Set;     Right : in Element) return Set;
   function "+" (Left : in Element; Right : in Set)     return Set;
   function Union (Left : in Set;     Right : in Element) return Set renames "+";
   function Union (Left : in Element; Right : in Set)     return Set renames "+";

   function "*" (Left : in Set; Right : in Set) return Set; -- Intersection
   function Intersection (Left : in Set; Right : in Set) return Set renames "*";

   function "-" (Left : in Set; Right : in Set) return Set; -- Difference
   function Difference (Left : in Set; Right : in Set) return Set renames "-";

   function "-" (Left : in Set; Right : in Element) return Set; -- Performs the difference of Left and a Set consisting of Right
   function Difference (Left : in Set; Right : in Element) return Set renames "-";

   function "/" (Left : in Set; Right : in Set) return Set; -- Symmetric difference
   function Symmetric_Differece (Left : in Set; Right : in Set) return Set renames "/";

   function "<=" (Left : in Set; Right : in Set) return Boolean; -- Subset
   function Subset (Left : in Set; Right : in Set) return Boolean renames "<=";

   function "<" (Left : in Set; Right : in Set) return Boolean with
      Post => "<"'Result = (Left <= Right and Left.Size < Right.Size); -- Proper subset
   function Proper_Subset (Left : in Set; Right : in Set) return Boolean renames "<";

   function ">=" (Left : in Set; Right : in Set) return Boolean; -- Superset
   function Superset (Left : in Set; Right : in Set) return Boolean renames ">=";

   function ">" (Left : in Set; Right : in Set) return Boolean with
      Post => ">"'Result = (Left >= Right and Left.Size > Right.Size); -- Proper superset
   function Proper_Superset (Left : in Set; Right : in Set) return Boolean renames ">";

   function Member (Group : in Set; Item : in Element) return Boolean; -- Membership

   type Member_List is array (Positive range <>) of Element;

   function New_Set (List : Member_List) return Set;
   -- Allows a sort of Set literal:
   -- My_Set := New_Set ( (An_Element, Another_Element, Yet_A_3rd_Element) );

   function Size (Group : in Set) return Natural; -- Number in members
private -- PragmARC.Set_Discrete
   type Set_List is array (Element) of Boolean with Component_Size => 1;

   type Set is tagged record
      Value : Set_List := Set_List'(others => False);
   end record;

   Empty : constant Set := Set'(Value => Set_List'(others => False) );
   Full  : constant Set := Set'(Value => Set_List'(others => True ) );
end PragmARC.Data_Structures.Sets.Discrete;
