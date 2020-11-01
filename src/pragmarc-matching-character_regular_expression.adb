-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.0--Provide ranges in classes
--
package body PragmARC.Matching.Character_Regular_Expression is
   function Expanded_Ranges (Pattern : String) return String is
      function Expanded_Ranges (Start : Positive; In_Class : Boolean := False) return String;
      -- Expands any ranges in Pattern (Start .. Pattern'Last)
      -- Calls itself after expanding a range

      function Expanded_Ranges (Start : Positive; In_Class : Boolean := False) return String is
         function Expanded (Low : Character; High : Character) return String;
         -- Returns all Characters in Low .. High in order

         function Expanded (Low : Character; High : Character) return String is
            Result : String (1 .. Character'Pos (High) - Character'Pos (Low) + 1);
            Index  : Positive := 1;
         begin -- Expanded
            All_Characters : for C in Low .. High loop
               Result (Index) := C;
               Index := Index + 1;
            end loop All_Characters;

            return Result;
         end Expanded;

         Class : Boolean := In_Class;
      begin -- Expanded_Ranges
         Check_All : for I in Start .. Pattern'Last loop
            case Pattern (I) is
            when Start_Class_Item =>
               if not Class and (I = Pattern'First or else Pattern (I - 1) /= Escape_Item) then
                  Class := True;
               end if;
            when Stop_Class_Item =>
               if Class and Pattern (I - 1) /= Escape_Item then
                  Class := False;
               end if;
            when '-' =>
               if Class then
                  if I = Pattern'Last then
                     raise Illegal_Pattern;
                  end if;

                  if Pattern (I - 1) /= Start_Class_Item and Pattern (I - 1) /= Escape_Item and Pattern (I + 1) /= Stop_Class_Item
                  then
                     return Pattern (Start .. I - 2) &
                            Expanded (Pattern (I - 1), Pattern (I + 1) ) &
                            Expanded_Ranges (I + 2, True);
                  end if;
               end if;
            when others =>
               null;
            end case;
         end loop Check_All;

         return Pattern (Start .. Pattern'Last); -- No ranges to expand
      end Expanded_Ranges;
   begin -- Expanded_Ranges
      return Expanded_Ranges (Pattern'First);
   end Expanded_Ranges;

   procedure Process (Pattern : in String; Processed : in out Processed_Pattern) is
      -- Empty
   begin -- Processe
      Regexp.Process (Pattern => Expanded_Ranges (Pattern), Processed => Processed);
   end Process;

   function Location (Pattern : String; Source : String) return Result is
      -- Empty
   begin -- Location
      return Regexp.Location (Expanded_Ranges (Pattern), Source);
   end Location;
end PragmARC.Matching.Character_Regular_Expression;
