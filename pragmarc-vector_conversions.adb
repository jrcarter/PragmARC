-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides missing operations for converting vectors to and from their fixed equivalents
-- (equivalent to To_String and To_Unbounded_String for unbounded strings)
--
-- History
-- 2018 Jun 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Vector_Conversions is
   function To_Fixed (Vector : Vectors.Vector) return Fixed is
      Result : Fixed (Index'First .. Vector.Last_Index);
   begin -- To_Fixed
      All_Elements : for I in Result'Range loop
         Result (I) := Vector.Element (I);
      end loop All_Elements;

      return Result;
   end To_Fixed;

   function To_Vector (List : Fixed) return Vectors.Vector is
      Result : Vectors.Vector;
   begin -- To_Vector
      Result.Reserve_Capacity (Capacity => List'Length);

      All_Elements : for I in List'Range loop
         Result.Append (New_Item => List (I) );
      end loop All_Elements;

      return Result;
   end To_Vector;
end PragmaRC.Vector_Conversions;
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
