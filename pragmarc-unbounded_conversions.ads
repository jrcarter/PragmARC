-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Unary conversion operators for unbounded strings.
--
-- History:
-- 2018 Mar 15     J. Carter          V1.0--Initial release
--
with Ada.Strings.Unbounded;

package PragmARC.Unbounded_Conversions is
   function "+" (Right : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (Right : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
end PragmARC.Unbounded_Conversions;
