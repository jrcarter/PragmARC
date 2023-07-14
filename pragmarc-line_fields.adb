-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Parse a String into fields based on a separator Character
--
-- History:
-- 2022 Aug 01     J. Carter          V1.0--Initial version
--
with Ada.Strings.Fixed;
with PragmARC.Conversions.Unbounded_Strings;

package body PragmARC.Line_Fields is
   use Ada.Strings.Unbounded;
   use PragmARC.Conversions.Unbounded_Strings;

   function Parsed (Line : String; Separator : Character := ' ') return Line_Field_Info is
      function Quote_Reduced (Line : in String) return String;
      -- Converts double quotes (Charles ""Buddy"" Holley) to single (Charles "Buddy" Holley)

      function Quote_Reduced (Line : in String) return String is
         Pos : constant Natural := Ada.Strings.Fixed.Index (Line, "" & '"');
      begin -- Quote_Reduced
         if Pos = 0 then
            return Line;
         end if;

         if Pos = Line'Last or else Line (Pos + 1) /= '"' then
            return Line (Line'First .. Pos) & Quote_Reduced (Line (Pos + 1 .. Line'Last) );
         end if;

         return Line (Line'First .. Pos) & Quote_Reduced (Line (Pos + 2 .. Line'Last) );
      end Quote_Reduced;

      Result : Line_Field_Info := (Raw => +Line, Field => <>);
      Start  : Natural         := Line'First;
      Stop   : Positive;
   begin -- Parsed
      All_Fields : loop
         if Separator = ' ' then
            Start := Ada.Strings.Fixed.Index_Non_Blank (Line (Start .. Line'Last) );
         end if;

         exit All_Fields when Start = 0 or Start > Line'Last;

         Stop := Start;

         Find_Separator : loop
            exit Find_Separator when Stop > Line'Last or else Line (Stop) = Separator;

            if Line (Stop) = '"' then -- Find matching "
               Find_Quote : loop
                  Stop := Stop + 1;

                  exit Find_Quote when Line (Stop) = '"';
               end loop Find_Quote;
            end if;

            Stop := Stop + 1;
         end loop Find_Separator;

         if Line (Start) = '"' then
            Result.Field.Append (New_Item => +Quote_Reduced (Line (Start + 1 .. Stop - 2) ) );
         else
            Result.Field.Append (New_Item => +Line (Start .. Stop - 1) );
         end if;

         Start := Stop + 1;
      end loop All_Fields;

      return Result;
   end Parsed;
end PragmARC.Line_Fields;
