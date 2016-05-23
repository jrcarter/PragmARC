-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic heap sort
--
-- History:
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2004 Sep 01     J. Carter          V1.0--Initial release
--
procedure PragmARC.Sort_Heap (Set : in out Sort_Set) is
   procedure Swap (Left : in out Element; Right : in out Element) is
      Temp : Element;
   begin -- Swap
      Temp := Left;
      Left := Right;
      Right := Temp;
   end Swap;
   pragma Inline (Swap);

   function ">=" (Left : Element; Right : Element) return Boolean is
      -- Empty
   begin -- ">="
      return not (Left < Right);
   end ">=";
   pragma Inline (">=");

   function "=" (Left : Element; Right : Element) return Boolean is
      -- Empty
   begin -- "="
      return (not (Left < Right) ) and (not (Right < Left) );
   end "=";
   pragma Inline ("=");

   function ">" (Left : Element; Right : Element) return Boolean is
      -- Empty
   begin -- ">"
      return Left /= Right and (not (Left < Right) );
   end ">";
   pragma Inline (">");

   function "<=" (Left : Element; Right : Element) return Boolean is
      -- Empty
   begin -- "<="
      return Left < Right or Left = Right;
   end "<=";
   pragma Inline ("<=");

   -- Using Index'Pos, we adjust by Index'Pos (Set'First) to obtain zero-based indexing.
   -- Node I's children are at 2 * I + 1 and 2 * I + 2. Its parent is at (I - 1) / 2.

   procedure Make_Heap (Set : in out Sort_Set) is
   -- Turn Set into a heap.
      procedure Extend_Heap (Set : in out Sort_Set) is
      -- Assumes Set (Set'First .. Index'Pred (Set'Last) ) is a heap.
      -- Adds Set (Set'Last) to this heap.
         Child  : Index := Set'Last;
         Parent : Index := Index'Val ( (Index'Pos (Child) - Index'Pos (Set'First) - 1) / 2 + Index'Pos (Set'First) );
      begin -- Extend_Heap
         Move_Child : loop
            exit Move_Child when Set (Child) <= Set (Parent);

            Swap (Left => Set (Parent), Right => Set (Child) );
            Child := Parent;

            exit Move_Child when Parent <= Set'First;

            Parent := Index'Val ( (Index'Pos (Parent) - Index'Pos (Set'First) - 1) / 2 + Index'Pos (Set'First) );
         end loop Move_Child;
      end Extend_Heap;
   begin -- Make_Heap
      Build : for I in Index'Succ (Set'First) .. Set'Last loop
         Extend_Heap (Set => Set (Set'First .. I) );
      end loop Build;
   end Make_Heap;

   procedure Sort (Set : in out Sort_Set) is
   -- Sorts Set, which is a heap
      procedure Reheap (Set : in out Sort_Set) is
      -- Converts the almost-heap in Set to a heap
         Parent : Index := Set'First;
         Child  : Index := Index'Succ (Parent);
      begin -- Reheap
         Move_Root : loop
            if Index'Pos (Child) - Index'Pos (Set'First) + 1 <= Set'Length - 1 and then
               Set (Child) < Set (Index'Succ (Child) )
            then
               Child := Index'Succ (Child);
            end if; -- Now Child is larger of the 2 children

            exit Move_Root when Child not in Set'range or Parent not in Set'range;
            exit Move_Root when Set (Child) <= Set (Parent);

            Swap (Left => Set (Parent), Right => Set (Child) );
            Parent := Child;

            exit Move_Root when 2 * (Index'Pos (Parent) - Index'Pos (Set'First) ) + 1 + Index'Pos (Set'First) > Set'Length - 1;

            Child := Index'Val (2 * (Index'Pos (Parent) - Index'Pos (Set'First) ) + 1 + Index'Pos (Set'First) );
         end loop Move_Root;
      end Reheap;
   begin -- Sort
      Move_Biggest : for I in reverse Index'Succ (Set'First) .. Set'Last loop
         Swap (Set (Set'First), Set (I) );
         Reheap (Set => Set (Set'First .. Index'Pred (I) ) );
      end loop Move_Biggest;
   end Sort;
begin -- PragmARC.Sort_Heap
   if Set'Length <= 1 then -- Already sorted
      return;
   end if;

   Make_Heap (Set => Set);
   Sort (Set => Set);
end PragmARC.Sort_Heap;
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
