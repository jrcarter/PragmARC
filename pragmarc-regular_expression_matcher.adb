-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.3--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.2--Added exception handler to Destroy
-- 2001 Feb 01     J. Carter          V1.1--Improve robustness and return length of pattern matched
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Unchecked_Deallocation;

use Ada;
package body PragmARC.Regular_Expression_Matcher is
   procedure Destroy (Pattern : in out Processed_Pattern) is
      procedure Free is new Unchecked_Deallocation (Object => Class_Info,           Name => Class_Ptr);
      procedure Free is new Unchecked_Deallocation (Object => Expanded_Pattern_Set, Name => Processed_Pattern_Ptr);
   begin -- Destroy
      if Pattern.Ptr = null then
         return;
      end if;

      Free_Classes : for I in Pattern.Ptr'range loop
         Free (X => Pattern.Ptr (I).Class_Data);
      end loop Free_Classes;

      Free (X => Pattern.Ptr);
   exception -- Destroy
   when others =>
      null;
   end Destroy;

   procedure Finalize (Object : in out Processed_Pattern) renames Destroy;

   procedure Process (Pattern : in Item_Set; Processed : in out Processed_Pattern) is
      type State_Id is (Building_Pattern, Building_Class, Escape_Pattern, Escape_Class);

      State          : State_Id := Building_Pattern;
      Pattern_Index  : Index    := Pattern'First;
      Expanded_Index : Positive := 1;
      Last_Index     : Natural  := 0;
   begin -- Process
      Destroy (Pattern => Processed);
      Processed.Ptr := new Expanded_Pattern_Set (1 .. Pattern'Length + 1);

      Build : loop
         case State is
         when Building_Pattern =>
            if Pattern (Pattern_Index) = Any_Item then
               if not Processed.Ptr (Expanded_Index).Un_Negated then
                  raise Illegal_Pattern;
               else
                  Processed.Ptr (Expanded_Index).Kind := Any;
                  Last_Index := Expanded_Index;
                  Expanded_Index := Expanded_Index + 1;
               end if;
            elsif Pattern (Pattern_Index) = Escape_Item then
               State := Escape_Pattern;
            elsif Pattern (Pattern_Index) = Not_Item then
               if Pattern_Index < Pattern'Last and Processed.Ptr (Expanded_Index).Un_Negated then
                  Processed.Ptr (Expanded_Index).Un_Negated := False;
               else
                  raise Illegal_Pattern;
               end if;
            elsif Pattern (Pattern_Index) = Closure_Item then
               if Pattern_Index < Pattern'Last and not Processed.Ptr (Expanded_Index).Closure then
                  Processed.Ptr (Expanded_Index).Closure := True;
               else
                  raise Illegal_Pattern;
               end if;
            elsif Pattern (Pattern_Index) = Start_Class_Item then
               Processed.Ptr (Expanded_Index).Kind := Class;
               Processed.Ptr (Expanded_Index).Class_Data := new Class_Info (Max_Elems => Pattern'Length - 2);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
               State := Building_Class;
            elsif Pattern (Pattern_Index) = Stop_Class_Item then
               raise Illegal_Pattern;
            else -- A literal
               Processed.Ptr (Expanded_Index).Kind := Literal;
               Processed.Ptr (Expanded_Index).Value := Pattern (Pattern_Index);
               Last_Index := Expanded_Index;
               Expanded_Index := Expanded_Index + 1;
            end if;
         when Building_Class =>
            if Pattern (Pattern_Index) = Escape_Item then
               State := Escape_Class;
            elsif Pattern (Pattern_Index) = Stop_Class_Item then
               if Processed.Ptr (Last_Index).Class_Data.Num_Elems > 0 then
                  State := Building_Pattern;
               else
                  raise Illegal_Pattern;
               end if;
            else -- A literal
               Processed.Ptr (Last_Index).Class_Data.Num_Elems := Processed.Ptr (Last_Index).Class_Data.Num_Elems + 1;
               Processed.Ptr (Last_Index).Class_Data.Element (Processed.Ptr (Last_Index).Class_Data.Num_Elems) :=
                  Pattern (Pattern_Index);
            end if;
         when Escape_Pattern =>
            Processed.Ptr (Expanded_Index).Kind := Literal;
            Processed.Ptr (Expanded_Index).Value := Pattern (Pattern_Index);
            Last_Index := Expanded_Index;
            Expanded_Index := Expanded_Index + 1;
            State := Building_Pattern;
         when Escape_Class =>
            Processed.Ptr (Last_Index).Class_Data.Num_Elems := Processed.Ptr (Last_Index).Class_Data.Num_Elems + 1;
            Processed.Ptr (Last_Index).Class_Data.Element (Processed.Ptr (Last_Index).Class_Data.Num_Elems) :=
               Pattern (Pattern_Index);
            State := Building_Class;
         end case;

         if Pattern_Index < Pattern'Last then
            Pattern_Index := Index'Succ (Pattern_Index);
         elsif State /= Building_Pattern then
            raise Illegal_Pattern;
         else
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
         -- Empty
      begin -- Match
         case Pattern.Ptr (P_Index).Kind is
         when Literal =>
            if Pattern.Ptr (P_Index).Un_Negated then
               return Pattern.Ptr (P_Index).Value = Source (S_Index);
            else
               return Pattern.Ptr (P_Index).Value /= Source (S_Index);
            end if;
         when Class =>
            if Pattern.Ptr (P_Index).Un_Negated then
               Search_True : for Elem_Num in 1 .. Pattern.Ptr (P_Index).Class_Data.Num_Elems loop
                  if Pattern.Ptr (P_Index).Class_Data.Element (Elem_Num) = Source (S_Index) then
                     return True;
                  end if;
               end loop Search_True;

               return False;
            else
               Search_False : for Elem_Num in 1 .. Pattern.Ptr (P_Index).Class_Data.Num_Elems loop
                  if Pattern.Ptr (P_Index).Class_Data.Element (Elem_Num) = Source (S_Index) then
                     return False;
                  end if;
               end loop Search_False;

               return True;
            end if;
         when Any =>
            return True;
         when others =>
            raise Illegal_Pattern;
         end case;
      end Match;

      function Locate (P_First : Positive; P_Last : Positive; S_First : Index; S_Last : Index) return Result is
      -- Matches the pattern given by Pattern.Ptr (P_First .. P_Last) to the "string" given by
      -- Source (S_First .. S_Last).  Match is "anchored;" that is, it must match starting at S_First in Source

         Source_Index  : Index    := S_First;
         Total_Matches : Natural  := 0;
         Location      : Result;
         Temp_Index    : Index;
         P_Index       : Positive := P_Last;
         Match_Length  : Natural  := 0;
      begin -- Locate
         Check_All : for Expanded_Index in P_First .. P_Last loop
            if Pattern.Ptr (Expanded_Index).Kind = Stop then
               return Result'(Found => True, Start => S_First, Length => Match_Length);
            elsif Pattern.Ptr (Expanded_Index).Closure then
               Count_Matches : for Index in Source_Index .. S_Last loop
                  exit Count_Matches when not Match (P_Index => Expanded_Index, S_Index => Index);

                  Total_Matches := Total_Matches + 1;
               end loop Count_Matches;

               -- Check for having matched all of Source
               if Index'Pos (Source_Index) + Total_Matches > Index'Pos (S_Last) then
                  if Pattern.Ptr (Expanded_Index + 1).Kind = Stop then
                     return Result'(Found => True, Start => S_First, Length => Match_Length + Total_Matches);
                  else
                     Total_Matches := Total_Matches - 1;
                  end if;
               end if;

               Reduce_Matches : loop
                  exit Reduce_Matches when Total_Matches <= 0;

                  Temp_Index := Index'Val (Index'Pos (Source_Index) + Total_Matches);
                  Location := Locate (Expanded_Index + 1, P_Last, Temp_Index, S_Last);

                  if Location.Found then
                     return Result'(Found => True, Start => S_First, Length => Match_Length + Total_Matches + Location.Length);
                  else
                     Total_Matches := Total_Matches - 1;
                  end if;
               end loop Reduce_Matches;
            elsif Match (P_Index => Expanded_Index, S_Index => Source_Index) then
               Match_Length := Match_Length + 1;

               if Source_Index < S_Last then
                  Source_Index := Index'Succ (Source_Index);
               else
                  P_Index := Expanded_Index + 1;

                  exit Check_All;
               end if;
            else
               return Result'(Found => False);
            end if;
         end loop Check_All;

         if P_Index > P_Last or else Pattern.Ptr (P_Index).Kind = Stop then
            return Result'(Found => True, Start => S_First, Length => Match_Length);
         else -- Exhausted Source before Pattern; only matches if rest of Pattern is all closures
            Check_Rest : for Expanded_Index in P_Index .. P_Last loop
               if not Pattern.Ptr (Expanded_Index).Closure and Pattern.Ptr (Expanded_Index).Kind /= Stop then
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
      Search : for Position in Source'range loop
         Local := Locate (P_First => Pattern.Ptr'First, P_Last => Pattern.Ptr'Last, S_First => Position, S_Last => Source'Last);

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
