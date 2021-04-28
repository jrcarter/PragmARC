-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Jun 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Conversions.Vectors is
   function To_Fixed (Vector : in Unbounded.Vector) return Fixed is
      Result : Fixed (Index'First .. Vector.Last_Index);
   begin -- To_Fixed
      All_Elements : for I in Result'Range loop
         Result (I) := Vector.Element (I);
      end loop All_Elements;

      return Result;
   end To_Fixed;

   function To_Vector (List : in Fixed) return Unbounded.Vector is
      Result : Unbounded.Vector;
   begin -- To_Vector
      Result.Reserve_Capacity (Capacity => List'Length);

      All_Elements : for I in List'Range loop
         Result.Append (New_Item => List (I) );
      end loop All_Elements;

      return Result;
   end To_Vector;
end PragmaRC.Conversions.Vectors;
