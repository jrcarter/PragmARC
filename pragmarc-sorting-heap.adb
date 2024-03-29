-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generic heap sort
--
-- History:
-- 2023 Mar 01     J. Carter          V2.2--Use PragmARC.Comparisons
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.3--Sequences indexed by integers
-- 2018 May 01     J. Carter          V1.2--Removed unused comparison operators
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
with PragmARC.Comparisons;

procedure PragmARC.Sorting.Heap (Set : in out Sort_Set) is
   procedure Swap (Left : in out Element; Right : in out Element) with Inline,
      Post => Left = Right'Old and Right = Left'Old;
   -- Swaps Left and Right

   package Comparisons is new PragmARC.Comparisons (T => Element);
   use Comparisons;

   procedure Make_Heap (Set : in out Sort_Set);
   -- Turn Set into a heap

   procedure Sort (Set : in out Sort_Set);
   -- Sorts Set, which is a heap

   procedure Swap (Left : in out Element; Right : in out Element) is
      Temp : constant Element := Left;
   begin -- Swap
      Left := Right;
      Right := Temp;
   end Swap;

   -- We adjust by Set'First to obtain zero-based indexing.
   -- Node I's children are at 2 * I + 1 and 2 * I + 2. Its parent is at (I - 1) / 2.

   procedure Make_Heap (Set : in out Sort_Set) is
      procedure Extend_Heap (Set : in out Sort_Set);
      -- Assumes Set (Set'First .. Set'Last - 1) is a heap.
      -- Adds Set (Set'Last) to this heap.

      procedure Extend_Heap (Set : in out Sort_Set) is
         Child  : Index := Set'Last;
         Parent : Index := (Child - Set'First - 1) / 2 + Set'First;
      begin -- Extend_Heap
         Move_Child : loop
            exit Move_Child when Set (Child) <= Set (Parent);

            Swap (Left => Set (Parent), Right => Set (Child) );
            Child := Parent;

            exit Move_Child when Parent <= Set'First;

            Parent := (Parent - Set'First - 1) / 2 + Set'First;
         end loop Move_Child;
      end Extend_Heap;
   begin -- Make_Heap
      Build : for I in Set'First + 1 .. Set'Last loop
         Extend_Heap (Set => Set (Set'First .. I) );
      end loop Build;
   end Make_Heap;

   procedure Sort (Set : in out Sort_Set) is
      procedure Reheap (Set : in out Sort_Set);
      -- Converts the almost-heap in Set to a heap

      procedure Reheap (Set : in out Sort_Set) is
         Parent : Index := Set'First;
         Child  : Index := Parent + 1;
      begin -- Reheap
         Move_Root : loop
            if Child - Set'First + 1 <= Set'Length - 1 and Then Set (Child) < Set (Child + 1) then
               Child := Child + 1;
            end if; -- Now Child is larger of the 2 children

            exit Move_Root when Child not in Set'Range or Parent not in Set'Range;
            exit Move_Root when Set (Child) <= Set (Parent);

            Swap (Left => Set (Parent), Right => Set (Child) );
            Parent := Child;

            exit Move_Root when 2 * (Parent - Set'First) + 1 + Set'First > Set'Length - 1;

            Child := 2 * (Parent - Set'First) + 1 + Set'First;
         end loop Move_Root;
      end Reheap;
   begin -- Sort
      Move_Biggest : for I in reverse Set'First + 1 .. Set'Last loop
         Swap (Set (Set'First), Set (I) );
         Reheap (Set => Set (Set'First .. I - 1) );
      end loop Move_Biggest;
   end Sort;
begin -- PragmARC.Sorting.Heap
   if Set'Length <= 1 then -- Already sorted
      return;
   end if;

   Make_Heap (Set => Set);
   Sort (Set => Set);
end PragmARC.Sorting.Heap;
