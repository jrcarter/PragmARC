-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2019 Apr 01     J. Carter          V1.2--Require integer index
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
--------------------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V2.0--Package with sequential and parallel sorts
-- 2004 Sep 01     J. Carter          V1.3--Extracted insertion sort
-- 2002 Oct 01     J. Carter          V1.2--Use mode out to allow scalars
-- 2001 Feb 01     J. Carter          V1.1--Increased size of sets that are insertion sorted
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Sort_Insertion;
with System;
package body PragmARC.Sort_Quick_In_Place is
   procedure Exchange (Left : in out Element; Right : in out Element) is
      Temp : Element;
   begin -- Exchange
      Temp := Left;
      Left := Right;
      Right := Temp;
   end Exchange;
   pragma Inline (Exchange);

   -- Median of 3 pivot selection
   procedure Get_Pivot (Set : in out Sort_Set; Pivot : in out Element) is
      type Big is range System.Min_Int .. System.Max_Int;

      Mid_Index   : Index;
      Left_Value  : Element;
      Right_Value : Element;
   begin -- Get_Pivot
      if Big'Last - Big (Set'First) > Big (Set'Last) then
         Mid_Index := Index (Big (Set'First) / 2 + Big (Set'Last) / 2);
      else
         Mid_Index := Index ( (Big (Set'First) + Big (Set'Last) ) / 2);
      end if;

      Left_Value  := Set (Set'First);
      Pivot       := Set (Mid_Index);
      Right_Value := Set (Set'Last);

      if Pivot < Left_Value then
         Exchange (Left => Left_Value, Right => Pivot);
      end if;

      if Right_Value < Pivot then
         Exchange (Left => Pivot, Right => Right_Value);
      end if;

      if Pivot < Left_Value then
         Exchange (Left => Left_Value, Right => Pivot);
      end if; -- Now pivot is median

      Set (Set'First) := Left_Value;  -- Set (Set'First) gets value known <= Pivot
      Set (Set'Last)  := Right_Value; -- Set (Set'Last)  gets value known >= Pivot

      Set (Mid_Index)              := Set (Index'Pred (Set'Last) );
      Set (Index'Pred (Set'Last) ) := Pivot;
      -- Last 2 assigns put Pivot in Set (Set'Last - 1); previous value there put in center of Set
   end Get_Pivot;

   procedure Insertion_Sort is new PragmARC.Sort_Insertion (Element, Index, Sort_Set);

   function ">=" (Left : Element; Right : Element) return Boolean is
      -- Empty
   begin -- ">="
      return not (Left < Right);
   end ">=";
   pragma Inline (">=");

   -- Partitions Set (Front .. Back)
   procedure Partition (Set : in out Sort_Set; Front : in out Index; Back : in out Index; Pivot : in Element) is
      -- Empty
   begin -- Partition
      Both : loop
         Small : loop -- Increase Front until it points to something >= Pivot
            exit Small when Set (Front) >= Pivot;

            Front := Front + 1;
         end loop Small;

         Big : loop -- Decrease Back until it points to something <= Pivot
            exit Big when Pivot >= Set (Back);

            Back := Back - 1;
         end loop Big;

         if Front <= Back then -- Found out of order pair: Set (Front) >= Pivot and Set (Back) <= Pivot
            if Front < Back then
               Exchange (Left => Set (Front), Right => Set (Back) );
            end if;

            Front := Front + 1;
            Back  := Back - 1;
         end if;

         exit Both when Front > Back;
      end loop Both;

      Exchange (Left => Set (Set'Last - 1), Right => Set (Front) ); -- Put Pivot in its place
   end Partition;

   procedure Easy_Cases (Set : in out Sort_Set; Finished : out Boolean) is
      -- Empty
   begin -- Easy_Cases
      Finished := False;

      if Set'Length <= 1 then -- Set is already sorted
         Finished := True;

         return;
      end if;

      if Set'Length = 2 then                      -- Only 2 elements in Set
         if Set (Set'Last) < Set (Set'First) then -- They're out of order
            Exchange (Left => Set (Set'First), Right => Set (Set'Last) );
         end if; -- Now Set is sorted

         Finished := True;

         return;
      end if;

      if Set'Length < 21 then -- Use insertion sort on small sets
         Insertion_Sort (Set => Set);
         Finished := True;

         return;
      end if;
   end Easy_Cases;

   procedure Sort_Sequential (Set : in out Sort_Set) is
      Finished : Boolean;
      Front    : Index;
      Back     : Index;
      Pivot    : Element;
   begin -- Sort_Sequential
      Easy_Cases (Set => Set, Finished => Finished);

      if Finished then
         return;
      end if;

      Get_Pivot (Set => Set, Pivot => Pivot);
      Front := Set'First + 1;  -- Set (Set'First) is known to be <= Pivot
      Back  := Set'Last - 2;   -- Set (Set'Last)  is known to be >= Pivot & Set (Set'Last - 1) is the Pivot

      Partition (Set => Set, Front => Front, Back => Back, Pivot => Pivot);

      if Front - Set'First < Set'Last - Front then       -- Sort shorter set first
         Sort_Sequential (Set => Set (Set'First .. Front - 1) );
         Sort_Sequential (Set => Set (Front + 1 .. Set'Last) );
      else
         Sort_Sequential (Set => Set (Front + 1 .. Set'Last) );
         Sort_Sequential (Set => Set (Set'First .. Front - 1) );
      end if;
   end Sort_Sequential;

   procedure Sort_Parallel (Set : in out Sort_Set; Max_Tasks : in Natural := 1) is
      protected Task_Control is
         function Remaining return Natural;
         procedure Decrement;
      private -- Task_Control
         Count : Natural := Max_Tasks;
      end Task_Control;

      task type Sorter (First : Index; Last : Index);

      procedure Sort (First : in Index; Last : in Index) is
         Finished : Boolean;
         Front    : Index;
         Back     : Index;
         Pivot    : Element;
      begin -- Sort
         if Task_Control.Remaining < 1 then
            Sort_Sequential (Set => Set (First .. Last) );

            return;
         end if;

         Easy_Cases (Set => Set (First .. Last), Finished => Finished);

         if Finished then
            return;
         end if;

         Get_Pivot (Set => Set (First .. Last), Pivot => Pivot);
         Front := First + 1; -- Set (First) is known to be <= Pivot
         Back  := Last - 2;  -- Set (Last) is known to be >= Pivot & Set (Last - 1) is the Pivot

         Partition (Set => Set, Front => Front, Back => Back, Pivot => Pivot);
         Task_Control.Decrement;

         Make_Sorter : declare
            Left : Sorter (First => First, Last => Front - 1);
         begin -- Make_Sorter
            Sort (First => Index'Succ (Front), Last => Last);
         end Make_Sorter;
      end Sort;

      protected body Task_Control is
         function Remaining return Natural is
            -- Empty
         begin -- Remaining
            return Count;
         end Remaining;

         procedure Decrement is
            -- Empty
         begin -- Decrement
            if Count > 0 then
               Count := Count - 1;
            end if;
         end Decrement;
      end Task_Control;

      task body Sorter is
         -- Empty
      begin -- Sorter
         Sort (First => First, Last => Last);
      end Sorter;
   begin -- Sort_Parallel
      Sort (First => Set'First, Last => Set'Last);
   end Sort_Parallel;
end PragmARC.Sort_Quick_In_Place;
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
