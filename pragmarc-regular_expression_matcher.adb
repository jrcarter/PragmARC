-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2019 Sep 01     J. Carter          V1.6--Simplify class-matching logic
-- 2019 Apr 15     J. Carter          V1.5--Sequences indexed by integers; add anchor items to patterns
-- 2018 Aug 01     J. Carter          V1.4--Cleanup compiler warnings
-- 2016 Jun 01     J. Carter          V1.3--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.2--Added exception handler to Destroy
-- 2001 Feb 01     J. Carter          V1.1--Improve robustness and return length of pattern matched
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Regular_Expression_Matcher is
   procedure Process (Pattern : in Item_Set; Processed : in out Processed_Pattern) is
      type State_Id is (Building_Pattern, Building_Class, Escape_Pattern, Escape_Class);

      State          : State_Id := Building_Pattern;
      Pattern_Index  : Index    := Pattern'First;
      Expanded_Index : Positive := 1;
      Last_Index     : Natural  := 0;
      Pattern_Item   : Expanded_Pattern_Item;
   begin -- Process
      Processed.List.Clear;
      Processed.List.Append (New_Item => (others => <>), Count => Ada.Containers.Count_Type (Pattern'Length + 1) );

      Build : loop
         case State is
         when Building_Pattern =>
            if Pattern (Pattern_Index) = Any_Item then -- The *_Item formal constants are not static, so we can't use case here
               Pattern_Item := Processed.List.Element(Expanded_Index);

               if not Pattern_Item.Un_Negated then
                  raise Illegal_Pattern;
               end if;

               Pattern_Item.Kind := Any;
               Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
            elsif Pattern (Pattern_Index) = Escape_Item then
               State := Escape_Pattern;
            elsif Pattern (Pattern_Index) = Not_Item then
               Pattern_Item := Processed.List.Element(Expanded_Index);

               if Pattern_Index < Pattern'Last and Pattern_Item.Un_Negated then
                  Pattern_Item.Un_Negated := False;
                  Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               else
                  raise Illegal_Pattern;
               end if;
            elsif Pattern (Pattern_Index) = Closure_Item then
               Pattern_Item := Processed.List.Element(Expanded_Index);

               if Pattern_Index < Pattern'Last and not Pattern_Item.Closure then
                  Pattern_Item.Closure := True;
                  Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               else
                  raise Illegal_Pattern;
               end if;
            elsif Pattern (Pattern_Index) = Start_Class_Item then
               Pattern_Item := Processed.List.Element(Expanded_Index);
               Pattern_Item.Kind := Class;
               Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
               State := Building_Class;
            elsif Pattern (Pattern_Index) = Stop_Class_Item then
               raise Illegal_Pattern;
            elsif Pattern (Pattern_Index) = Begin_Set_Item then
               if Pattern_Index /= Pattern'First then
                  raise Illegal_Pattern;
               end if;

               Pattern_Item := Processed.List.Element(Expanded_Index);
               Pattern_Item.Kind := Beginning;
               Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
            elsif Pattern (Pattern_Index) = End_Set_Item then
               if Pattern_Index /= Pattern'Last then
                  raise Illegal_Pattern;
               end if;

               Pattern_Item := Processed.List.Element(Expanded_Index);
               Pattern_Item.Kind := Ending;
               Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
            else -- A literal
               Pattern_Item := Processed.List.Element(Expanded_Index);
               Pattern_Item.Kind := Literal;
               Pattern_Item.Value := Pattern (Pattern_Index);
               Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
            end if;
         when Building_Class =>
            if Pattern (Pattern_Index) = Escape_Item then
               State := Escape_Class;
            elsif Pattern (Pattern_Index) = Stop_Class_Item then
               Pattern_Item := Processed.List.Element(Last_Index);

               if Pattern_Item.Class_Data.Last_Index > 0 then
                  State := Building_Pattern;
               else
                  raise Illegal_Pattern;
               end if;
            else -- A literal
               Pattern_Item := Processed.List.Element(Last_Index);
               Pattern_Item.Class_Data.Append (New_Item => Pattern (Pattern_Index) );
               Processed.List.Replace_Element (Index => Last_Index, New_Item => Pattern_Item);
            end if;
         when Escape_Pattern =>
            Pattern_Item := Processed.List.Element (Expanded_Index);
            Pattern_Item.Kind := Literal;
            Pattern_Item.Value := Pattern (Pattern_Index);
            Processed.List.Replace_Element (Index => Expanded_Index, New_Item => Pattern_Item);
            Last_Index := Expanded_Index;
            Expanded_Index := Expanded_Index + 1;
            State := Building_Pattern;
         when Escape_Class =>
            Pattern_Item := Processed.List.Element (Last_Index);
            Pattern_Item.Class_Data.Append (New_Item => Pattern (Pattern_Index) );
            Processed.List.Replace_Element (Index => Last_Index, New_Item => Pattern_Item);
            State := Building_Class;
         end case;

         if Pattern_Index < Pattern'Last then
            Pattern_Index := Pattern_Index + 1;
         elsif State /= Building_Pattern then
            raise Illegal_Pattern;
         else
            Delete_Unused : for I in 1 .. Processed.List.Last_Index loop
               if Processed.List.Element (I).Kind = Stop then
                  Processed.List.Delete (Index => I + 1, Count => Ada.Containers.Count_Type (Processed.List.Last_Index - I) );

                  exit Delete_Unused;
               end if;
            end loop Delete_Unused;

            return;
         end if;
      end loop Build;
   exception -- Process
   when Constraint_Error =>
      raise Illegal_Pattern;
   end Process;

   function Location (Pattern : Processed_Pattern; Source : Item_Set) return Result is
      Local : Result;

      function Match (P_Index : Positive; S_Index : Index) return Boolean is -- Matches one Pattern element with one Item
         Pattern_Item : constant Expanded_Pattern_Item := Pattern.List.Element (P_Index);
      begin -- Match
         case Pattern_Item.Kind is
         when Literal =>
            if Pattern_Item.Un_Negated then
               return Pattern_Item.Value = Source (S_Index);
            else
               return Pattern_Item.Value /= Source (S_Index);
            end if;
         when Class =>
            Search_Match : for Elem_Num in 1 .. Pattern_Item.Class_Data.Last_Index loop
               if Pattern_Item.Class_Data.Element (Elem_Num) = Source (S_Index) then
                  return Pattern_Item.Un_Negated;
               end if;
            end loop Search_Match;

            return not Pattern_Item.Un_Negated;
         when Any =>
            return True;
         when others =>
            raise Illegal_Pattern;
         end case;
      end Match;

      function Locate (P_First : Positive; P_Last : Positive; S_First : Index) return Result is
      -- Matches the pattern given by Pattern.Ptr (P_First .. P_Last) to the "string" given by
      -- Source (S_First .. S_Last).  Match is "anchored;" that is, it must match starting at S_First in Source
         S_Last : constant Index'Base := Source'Last;

         Source_Index  : Index    := S_First;
         Total_Matches : Natural  := 0;
         Location      : Result;
         Temp_Index    : Index;
         P_Index       : Positive := P_Last;
         Match_Length  : Natural  := 0;
         Pattern_Item  : Expanded_Pattern_Item;
      begin -- Locate
         Check_All : for Expanded_Index in P_First .. P_Last loop
            Pattern_Item := Pattern.List.Element (Expanded_Index);

            if Pattern_Item.Kind = Stop then
               return Result'(Found => True, Start => S_First, Length => Match_Length);
            elsif Pattern_Item.Closure then
               Count_Matches : for Index in Source_Index .. S_Last loop
                  exit Count_Matches when not Match (Expanded_Index, Index);

                  Total_Matches := Total_Matches + 1;
               end loop Count_Matches;

               -- Check for having matched all of Source
               if Integer (Source_Index) + Total_Matches > Integer (S_Last) then
                  Pattern_Item := Pattern.List.Element (Expanded_Index + 1);

                  if Pattern_Item.Kind = Stop or Pattern_Item.Kind = Ending then
                     return Result'(Found => True, Start => S_First, Length => Match_Length + Total_Matches);
                  else
                     Total_Matches := Total_Matches - 1;
                  end if;
               end if;

               Reduce_Matches : loop
                  exit Reduce_Matches when Total_Matches <= 0;

                  Temp_Index := Index (Integer (Source_Index) + Total_Matches);
                  Location := Locate (Expanded_Index + 1, P_Last, Temp_Index);

                  if Location.Found then
                     return Result'(Found => True, Start => S_First, Length => Match_Length + Total_Matches + Location.Length);
                  else
                     Total_Matches := Total_Matches - 1;
                  end if;
               end loop Reduce_Matches;
            elsif Pattern_Item.Kind = Beginning then
               if Source_Index /= Source'First then
                  return Result'(Found => False);
               end if;

               return Locate (Expanded_Index + 1, P_Last, S_First);
            elsif Pattern_Item.Kind = Ending then
               return Result'(Found => False);
            elsif Match (Expanded_Index, Source_Index) then
               Match_Length := Match_Length + 1;

               if Source_Index < S_Last then
                  Source_Index := Source_Index + 1;
               else
                  P_Index := Expanded_Index + 1;

                  exit Check_All;
               end if;
            else
               return Result'(Found => False);
            end if;
         end loop Check_All;

         if P_Index <= P_Last then
            Pattern_Item := Pattern.List.Element (P_Index);
         end if;

         if P_Index > P_Last or else (Pattern_Item.Kind = Stop or (Pattern_Item.Kind = Ending and Source_Index = S_Last) ) then
            return Result'(Found => True, Start => S_First, Length => Match_Length);
         else -- Exhausted Source before Pattern; only matches if rest of Pattern is all closures
            Check_Rest : for Expanded_Index in P_Index .. P_Last loop
               Pattern_Item := Pattern.List.Element (Expanded_Index);

               if not Pattern_Item.Closure and Pattern_Item.Kind /= Stop then
                  return Result'(Found => False);
               end if;
            end loop Check_Rest;

            return Result'(Found => True, Start => S_First, Length => Match_Length);
         end if;
      exception -- Locate
      when Constraint_Error =>
         return Result'(Found => False);
      end Locate;
   begin -- Location
      if Source'Length = 0 then
         case Pattern.List.Element (1).Kind is
         when Beginning =>
            if Pattern.List.Last_Index = 1 or else (Pattern.List.Element (2).Kind = Stop or Pattern.List.Element (2).Kind = Ending)
            then
               return Result'(Found => True, Start => Source'First, Length => 0);
            end if;

            return Locate (1, Pattern.List.Last_Index, Source'First);
            -- Beginning followed by only closures (followed by an optional Ending) will match
         when Ending =>
            return Result'(Found => True, Start => Source'First, Length => 0);
         when others => -- Only closures (followed by an optional Ending) will match
            return Locate (1, Pattern.List.Last_Index, Source'First);
         end case;
      end if;

      Search : for Position in Source'range loop
         Local := Locate (1, Pattern.List.Last_Index, Position);

         if Local.Found then
            return Local;
         end if;
      end loop Search;

      return Result'(Found => False);
   end Location;

   function Location (Pattern : Item_Set; Source : Item_Set) return Result is
      Processed : Processed_Pattern;
   begin -- Location
      Process (Pattern => Pattern, Processed => Processed);

      return Location (Pattern => Processed, Source => Source);
   end Location;
end PragmARC.Regular_Expression_Matcher;
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
