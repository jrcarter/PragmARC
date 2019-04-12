-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Instantiation of PragmARC.Regular_Expression_Matcher for strings
--
-- History:
-- 2019 Apr 15     J. Carter          V1.0--Provide ranges in classes
--
with Ada.Strings.Fixed;

package body PragmARC.Character_Regular_Expression_Matcher is
   function Expanded_Ranges (Pattern : String) return String is
      function Expanded_Ranges (Start : Positive) return String;
      -- Expands any ranges in Pattern (Start .. Pattern'Last) with Start > Pattern'First and Pattern (Start) part of a class
      -- Calls itself after expanding a range

      function Expanded_Ranges (Start : Positive) return String is
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

         Class : Boolean := True;
      begin -- Expanded_Ranges
         Check_All : for I in Start .. Pattern'Last loop
            case Pattern (I) is
            when Start_Class_Item =>
               if not Class and Pattern (I - 1) /= Escape_Item then
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
                            Expanded_Ranges (Pattern (I + 2 .. Pattern'Last) );
                  end if;
               end if;
            when others =>
               null;
            end case;
         end loop Check_All;

         return Pattern (Start .. Pattern'Last); -- No ranges to expand
      end Expanded_Ranges;

      Index : Natural := Pattern'First;
   begin -- Expanded_Ranges
      Find_Class : loop
         Index := Ada.Strings.Fixed.Index (Pattern (Index .. Pattern'Last), (1 => Start_Class_Item) );

         if Index = 0 then
            return Pattern; -- No classes in Pattern
         end if;

         if Index = Pattern'First or else Pattern (Index - 1) /= Escape_Item then -- Start of a class
            return Pattern (Pattern'First .. Index) & Expanded_Ranges (Index + 1);
         end if;

         Index := Index + 1;
      end loop Find_Class;
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
end PragmARC.Character_Regular_Expression_Matcher;
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
