-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Quick_Searcher is
   function Quick_Search (Pattern : String; Source : String) return Result is
      type Shift_Table_Set is array (Character) of Natural;

      Shift_Table   : Shift_Table_Set := Shift_Table_Set'(others => Pattern'Length + 1);
      Source_Index  : Positive := Source'First;
      Pattern_Index : Positive;
   begin -- Quick_Search
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
   exception -- Quick_Search
   when others =>
      return Result'(Found => False);
   end Quick_Search;
end PragmARC.Quick_Searcher;
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
