-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Compare two Strings in title order, ignoring an initial article
--
-- History:
-- 2023 Sep 15     J. Carter          V1.1--Require space in article string
-- 2022 Aug 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Indefinite_Vectors;

package PragmARC.Title_Comparisons is
   package Article_Lists is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);
   subtype Article_List is Article_Lists.Vector;

   function English return Article_List; -- "a ", "an ", and "the "
   function French  return Article_List; -- "un ", "une ", "le ", "la ", "les ", and "l'"
   -- Note that an article that is followed by a space must include the trailing space (English "a "), while an article that is not
   -- followed by a space does not have one (French "l'")

   function Less    (Left : in String; Right : in String; Article : in Article_List := English) return Boolean;
   function Greater (Left : in String; Right : in String; Article : in Article_List := English) return Boolean;
   -- Return True if their arguments are in that title order, ignoring an initial Article; False otherwise
end PragmARC.Title_Comparisons;
