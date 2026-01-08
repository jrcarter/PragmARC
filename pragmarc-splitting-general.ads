-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Parse a Sequence into fields based on a separator Element
-- (Generalization of the former PragmARC.Line_Fields)
--
-- History:
-- 2026 Jan 15     J. Carter          V1.0--Initial version
--
with Ada.Containers.Indefinite_Vectors;

generic -- PragmARC.Splitting.General
   type Element is private;
   type Index_Value is range <>; -- Lower bound of 1; see assertion below
   type Sequence is array (Index_Value range <>) of Element;

   with function "=" (Left : in Element; Right : in Element) return Boolean is <>;
package PragmARC.Splitting.General is
   pragma Assert (Index_Value'First = 1);

   package Field_Lists is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => Sequence);

   subtype Field_List is Field_Lists.Vector;

   function Parsed (Source : in Sequence; Separator : in Element; Quote : in Element; Merge_Separators : in Boolean := False)
   return Field_List with
      Pre => Source'Last < Index_Value'Last;
   -- Presumes that Source consists of fields speparated by Separator
   -- If Merge_Separators, then fields are separated by 1 or more Separators; otherwise, they are separated by exactly 1 Separator
   -- Fields surrounded by Quotes are quoted and may contain Separator. The surrounding Quotes are
   -- removed and any doubled Quotes are converted to single Quotes. Any field containing Quotes
   -- must have an even number of Quotes or Constraint_Error will be raised
   -- The result contains the fields parsed out of Source
   -- If Source begins with Separator, the first element of the result will be null
end PragmARC.Splitting.General;
