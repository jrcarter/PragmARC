-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generic insertion sort
--
-- History:
-- 2023 Mar 01     J. Carter          V2.2--Use PragmARC.Comparisons
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.2--Sequences indexed by integers
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
with PragmARC.Comparisons;

procedure PragmARC.Sorting.Insertion (Set : in out Sort_Set) is
   package Comparisons is new PragmARC.Comparisons (T => Element);
   use Comparisons;

   Temp : Element;
   J    : Index;
begin -- PragmARC.Sorting.Insertion
   if Set'Length <= 1 then -- Already sorted
      return;
   end if;

   Search : for I in Set'First + 1 .. Set'Last loop -- Invariant: Set (Set'First .. I - 1) is sorted
      Temp := Set (I);
      J := I;

      Insert : loop
         exit Insert when J <= Set'First or else Temp >= Set (J - 1);

         Set (J) := Set (J - 1);
         J := J - 1;
      end loop Insert;

      Set (J) := Temp;
   end loop Search;
end PragmARC.Sorting.Insertion;
