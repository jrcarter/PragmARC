-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Instantiation of PragmARC.Splitting.General for type String
-- (Functionality of the former PragmARC.Line_Fields)
--
-- History:
-- 2026 Jan 15     J. Carter          V1.0--Initial version
--
with PragmARC.Splitting.General;

package PragmARC.Splitting.Strings is
   package Implementation is new PragmARC.Splitting.General (Element => Character, Index_Value => Positive, Sequence => String);

   subtype Field_List is Implementation.Field_List;

   type Merge_Mode is (Auto, Yes, No);

   function Parsed (Line : in String; Separator : in Character := ' '; Merge : in Merge_Mode := Auto) return Field_List is
      (Implementation.Parsed (Source           => Line,
                              Separator        => Separator,
                              Quote            => '"',
                              Merge_Separators => (if Merge = Auto then Separator = ' ' else Merge = Yes) ) );
end PragmARC.Splitting.Strings;
