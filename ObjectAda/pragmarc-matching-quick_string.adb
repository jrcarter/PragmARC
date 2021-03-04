-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Matching.Quick_String is
   function Location (Pattern : String; Source : String) return Result is
      type Shift_Table_Set is array (Character) of Natural;

      Shift_Table   : Shift_Table_Set := Shift_Table_Set'(others => Pattern'Length + 1);
      Source_Index  : Positive := Source'First;
      Pattern_Index : Positive;
   begin -- Location
      Fill_Table : for I in Pattern'range loop
         Shift_Table (Pattern (I) ) := Pattern'Length - I + Pattern'First;
      end loop Fill_Table;

      Search_Source : loop
         exit Search_Source when Source_Index + Pattern'Length - 1 not in Source'range;

         Pattern_Index := Pattern'First;

         Scan_Pattern : loop
            exit Scan_Pattern when Pattern_Index not in Pattern'range or else
                 Pattern (Pattern_Index) /= Source (Source_Index + Pattern_Index - Pattern'First);

            Pattern_Index := Pattern_Index + 1;
         end loop Scan_Pattern;

         if Pattern_Index > Pattern'Last then -- found match
            return Result'(Found => True, Index => Source_Index);
         end if;

         Source_Index := Source_Index + Shift_Table (Source (Source_Index + Pattern'Length) );
      end loop Search_Source;

      return Result'(Found => False);
   exception -- Location
   when others =>
      return Result'(Found => False);
   end Location;
end PragmARC.Matching.Quick_String;
