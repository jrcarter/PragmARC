-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Oct 01     J. Carter          V1.0--Initial version
--
package body PragmARC.Real_Random_Ranges is
   function Random_Range (R : Uniform; Min : Real; Max : Real) return Real is
      -- Empty
   begin -- Random_Range
      return R * (Max - Min) + Min;
   end Random_Range;

   function Random_Int (R : Uniform; Min : Integer; Max : Integer) return Integer is
      Min_Work : constant Integer := Integer'Min (Min, Max);
      Max_Work : constant Integer := Integer'Max (Min, Max);
      -- assert: Min_Work <= Max_Work
      Value : constant Real := Random_Range (R, Real (Min_Work), Real (Max_Work) + 1.0);
      -- assert: Min_Work <= Value < Max_Work + 1
      -- assert: Min_Work <= Floor (Value) <= Max_Work
   begin -- Random_Int
      return Integer (Real'Floor (Value) );
   end Random_Int;

   function Normal (List : Normal_List; Mean : Real; Sigma : Real) return Real is
      Sum : Real := 0.0;
   begin -- Normal
      Add : for I in List'Range loop
         Sum := Sum + List (I);
      end loop Add;

      return Sigma * (Sum - 6.0) + Mean;
   end Normal;
end PragmARC.Real_Random_Ranges;
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
