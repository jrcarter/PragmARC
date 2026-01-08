-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2026 Jan 15     J. Carter          V1.0--Initial version
--
package body PragmARC.Splitting.General is
   function Parsed (Source : in Sequence; Separator : in Element; Quote : in Element; Merge_Separators : in Boolean := False)
   return Field_List is
      function Quote_Reduced (Source : in Sequence) return Sequence;
      -- Converts double Quotes to single

      function Quote_Reduced (Source : in Sequence) return Sequence is
         Pos : Index_Value := Source'First;
      begin -- Quote_Reduced
         Find_Quote : loop
            if Pos > Source'Last then
               return Source;
            end if;

            exit Find_Quote when Source (Pos) = Quote and (Pos = Source'Last or else Source (Pos + 1) = Quote);

            Pos := Pos + 1;
         end loop Find_Quote;

         return Source (Source'First .. Pos) & Quote_Reduced (Source (Pos + 2 .. Source'Last) );
      end Quote_Reduced;

      Result : Field_List;
      Start  : Index_Value := Source'First;
      Stop   : Index_Value;
   begin -- Parsed
      All_Fields : loop
         exit All_Fields when Start > Source'Last;

         Stop := Start;

         Find_Stop : loop
            exit Find_Stop when Stop > Source'Last or else Source (Stop) = Separator;

            if Source (Stop) = Quote then -- Find matching Quote
               Find_Quote : loop
                  Stop := Stop + 1;

                  exit Find_Quote when Source (Stop) = Quote;
               end loop Find_Quote;
            end if;

            Stop := Stop + 1;
         end loop Find_Stop;

         if Source (Start) = Quote then
            Result.Append (New_Item => Quote_Reduced (Source (Start + 1 .. Stop - 2) ) );
         else
            Result.Append (New_Item => Source (Start .. Stop - 1) );
         end if;

         if Merge_Separators then
            Skip_Multiple : loop
               exit Skip_Multiple when Stop >= Source'Last or else Source (Stop + 1) /= Separator;

               Stop := Stop + 1;
            end loop Skip_Multiple;
         end if;

         Start := Stop + 1;
      end loop All_Fields;

      return Result;
   end Parsed;
end PragmARC.Splitting.General;
