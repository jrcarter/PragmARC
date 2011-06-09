-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2005 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2005 Jul 01     J. Carter          V1.2--Eliminate assumption that A * B = B * A for Element
-- 2003 Sep 01     J. Carter          V1.1--Correct error in Matrix * Vector
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Matrix_Math is
   function "+" (Left : Matrix; Right : Matrix) return Matrix is
      Result : Matrix (Num_Rows => Left.Num_Rows, Num_Columns => Left.Num_Columns);
   begin -- "+"
      if Left.Num_Rows /= Right.Num_Rows or Left.Num_Columns /= Right.Num_Columns then
         raise Size_Mismatch;
      end if;

      Rows : for I in Result.Value'range (1) loop
         Cols : for J in Result.Value'range (2) loop
            Result.Value (I, J) := Left.Value (I, J) + Right.Value (I, J);
         end loop Cols;
      end loop Rows;

      return Result;
   end "+";

   function "-" (Left : Matrix; Right : Matrix) return Matrix is
      Result : Matrix (Num_Rows => Left.Num_Rows, Num_Columns => Left.Num_Columns);
   begin -- "-"
      if Left.Num_Rows /= Right.Num_Rows or Left.Num_Columns /= Right.Num_Columns then
         raise Size_Mismatch;
      end if;

      Rows : for I in Result.Value'range (1) loop
         Cols : for J in Result.Value'range (2) loop
            Result.Value (I, J) := Left.Value (I, J) - Right.Value (I, J);
         end loop Cols;
      end loop Rows;

      return Result;
   end "-";

   function "*" (Left : Matrix; Right : Matrix) return Matrix is
      Result : Matrix (Num_Rows => Left.Num_Rows, Num_Columns => Right.Num_Columns);
   begin -- "*"
      if Left.Num_Columns /= Right.Num_Rows then
         raise Size_Mismatch;
      end if;

      Result.Value := Matrix_Representation'(Left.Value'range (1) => (Right.Value'range (2) => Zero_Element) );

      Row : for I in Result.Value'range (1) loop
         Col : for J in Result.Value'range (2) loop
            Common : for K in Left.Value'range (2) loop
               Result.Value (I, J) := Result.Value (I, J) + Left.Value (I, K) * Right.Value (K, J);
            end loop Common;
         end loop Col;
      end loop Row;

      return Result;
   end "*";

   function "/" (Left : Matrix; Right : Matrix) return Matrix is
      -- null;
   begin -- "/"
      return Left * Invert (Right);
   end "/";

   function "*" (Left : Element; Right : Matrix) return Matrix is
      Result : Matrix (Num_Rows => Right.Num_Rows, Num_Columns => Right.Num_Columns);
   begin -- "*"
      Rows : for I in Right.Value'range (1) loop
         Cols : for J in Right.Value'range (2) loop
            Result.Value (I, J) := Left * Right.Value (I, J);
         end loop Cols;
      end loop Rows;

      return Result;
   end "*";

   function "*" (Left : Matrix; Right : Element) return Matrix is
      Result : Matrix (Num_Rows => Left.Num_Rows, Num_Columns => Left.Num_Columns);
   begin -- "*"
      Rows : for I in Left.Value'range (1) loop
         Cols : for J in Left.Value'range (2) loop
            Result.Value (I, J) := Left.Value (I, J) * Right;
         end loop Cols;
      end loop Rows;

      return Result;
   end "*";

   function "/" (Left : Matrix; Right : Element) return Matrix is
      Result : Matrix (Num_Rows => Left.Num_Rows, Num_Columns => Left.Num_Columns);
   begin -- "/"
      Rows : for I in Left.Value'range (1) loop
         Cols : for J in Left.Value'range (2) loop
            Result.Value (I, J) := Left.Value (I, J) / Right;
         end loop Cols;
      end loop Rows;

      return Result;
   end "/";

   function Transpose (Mat : Matrix) return Matrix is
      Result : Matrix (Num_Rows => Mat.Num_Columns, Num_Columns => Mat.Num_Rows);
   begin -- Transpose
      Rows : for I in Result.Value'range (1) loop
         Cols : for J in Result.Value'range (2) loop
            Result.Value (I, J) := Mat.Value (J, I);
         end loop Cols;
      end loop Rows;

      return Result;
   end Transpose;

   function Invert (Mat : Matrix) return Matrix is
      Cofactor_Matrix : Matrix (Num_Rows => Mat.Num_Columns, Num_Columns => Mat.Num_Rows);
      Determ          : Element; -- Determinant of Mat
   begin -- Invert
      if Mat.Num_Rows /= Mat.Num_Columns then
         raise Not_Square;
      end if;

      Determ := Determinant (Mat);

      if Determ = Zero_Element then
         raise Singular;
      end if;

      Cofactor_Cols : for I in Mat.Value'range (1) loop -- create transposed Matrix of cofactors
         Cofactor_Rows : for J in Mat.Value'range (2) loop
            Cofactor_Matrix.Value (J, I) := Cofactor (Mat, I, J);
         end loop Cofactor_Rows;
      end loop Cofactor_Cols;

      return Cofactor_Matrix / Determ;
   end Invert;

   function Determinant (Mat : Matrix) return Element is -- This function is mutually recursive with Cofactor
      -- null;
   begin -- Determinant
      if Mat.Num_Rows /= Mat.Num_Columns then
         raise Not_Square;
      end if;

      if Mat.Num_Rows < 1 then -- Matrix must be 1X1 or larger
         raise Too_Small;
      elsif Mat.Num_Rows = 1 then -- Determinant is the only Element
         return Mat.Value (1, 1);
      elsif Mat.Num_Rows = 2 then -- Can calculate this directly
         return Mat.Value (1, 1) * Mat.Value (2, 2) - Mat.Value (1, 2) * Mat.Value (2, 1);
      else -- Larger matrices calculated recursively
         Recurse : declare
            Result : Element := Zero_Element;
         begin -- Recurse
            Cols : for Col in Mat.Value'range (2) loop
               Result := Result + Mat.Value (1, Col) * Cofactor (Mat, 1, Col);
            end loop Cols;

            return Result;
         end Recurse;
      end if;
   end Determinant;

   function Cofactor (Mat : Matrix; I : Positive; J : Positive) return Element is
   -- This function is mutually recursive with Determinant
      Minor     : Matrix (Num_Rows => Mat.Num_Rows - 1, Num_Columns => Mat.Num_Rows - 1);
      Minor_Row : Positive;
      Minor_Col : Positive;
   begin -- Cofactor
      if Mat.Num_Rows /= Mat.Num_Columns then
         raise Not_Square;
      end if;

      Minor_Row := 1;

      All_Rows : for Row in Mat.Value'range (1) loop -- Create sub-Matrix excluding Ith row and Jth column
         if I /= Row then
            Minor_Col := 1;

            All_Cols : for Col in Mat.Value'range (2) loop
               if J /= Col then
                  Minor.Value (Minor_Row, Minor_Col) := Mat.Value (Row, Col);
                  Minor_Col := Minor_Col + 1;
               end if;
            end loop All_Cols;

            Minor_Row := Minor_Row + 1;
         end if;
      end loop All_Rows;

      return Neg_One_Element ** (I - J) * Determinant (Minor);
   end Cofactor;

   function Sub_Matrix (Mat : Matrix; From_Row : Positive; From_Col : Positive;
                           To_Row : Positive; To_Col : Positive)
   return Matrix is
      -- null;
   begin -- Sub_Matrix
      if From_Row > To_Row or From_Col > To_Col then
         Null_Matrix : declare
            Result : Matrix (Num_Rows => 0, Num_Columns => 0);
         begin -- Null_Matrix
            return Result;
         end Null_Matrix;
      else
         Not_Null : declare
            Result : Matrix (Num_Rows => To_Row - From_Row + 1, Num_Columns => To_Col - From_Col + 1);
         begin -- Not_Null
            Move_Rows : for I in Result.Value'range (1) loop
               Move_Cols : for J in Result.Value'range (2) loop
                  Result.Value (I, J) := Mat.Value (I + From_Row - 1, J + From_Col - 1);
               end loop Move_Cols;
            end loop Move_Rows;

            return Result;
         end Not_Null;
      end if;
   end Sub_Matrix;

   function "+" (Left : Vector; Right : Vector) return Vector is
      -- null;
   begin -- "+"
      return Vector'(Num_Elements => Left.Num_Elements, Value => Left.Value + Right.Value);
   end "+";

   function "-" (Left : Vector; Right : Vector) return Vector is
      -- null;
   begin -- "-"
      return Vector'(Num_Elements => Left.Num_Elements, Value => Left.Value - Right.Value);
   end "-";

   function "*" (Left : Vector; Right : Vector) return Element is
      Result : Element := Zero_Element;
   begin -- "*"
      if Left.Num_Elements /= Right.Num_Elements then
         raise Size_Mismatch;
      end if;

      All_Rows : for I in Left.Value.Value'range (1) loop
         Result := Result + Left.Value.Value (I, 1) * Right.Value.Value (I, 1);
      end loop All_Rows;

      return Result;
   end "*";

   function "*" (Left : Element; Right : Vector) return Vector is
      -- null;
   begin -- "*"
      return Vector'(Num_Elements => Right.Num_Elements, Value => Left * Right.Value);
   end "*";

   function "*" (Left : Vector; Right : Element) return Vector is
      -- null;
   begin -- "*"
      return Vector'(Num_Elements => Left.Num_Elements, Value => Left.Value * Right);
   end "*";

   function "/" (Left : Vector; Right : Element) return Vector is
      -- null;
   begin -- "/"
      return Vector'(Num_Elements => Left.Num_Elements, Value => Left.Value / Right);
   end "/";

   function "*" (Left : Matrix; Right : Vector) return Vector is
      -- null;
   begin -- "*"
      return Vector'(Num_Elements => Left.Num_Rows, Value => Left * Right.Value);
   end "*";

   function "*" (Left : Vector; Right : Matrix) return Matrix is
      -- null;
   begin -- "*"
      return Left.Value * Right;
   end "*";

   function Transpose (Vec : Vector) return Matrix is
      -- null;
   begin -- transpose
      return Transpose (Vec.Value);
   end Transpose;

   function Norm (Vec : Vector) return Element is
      -- null;
   begin -- norm
      return Sqrt (Vec * Vec);
   end Norm;

   function Cross (Left : Vector_3; Right : Vector_3) return Vector_3 is
      Result : Vector_3;
   begin -- cross
      Result.Value.Value (1, 1) := Left.Value.Value (2, 1) * Right.Value.Value (3, 1) -
                                   Left.Value.Value (3, 1) * Right.Value.Value (2, 1)
      ;
      Result.Value.Value (2, 1) := Left.Value.Value (3, 1) * Right.Value.Value (1, 1) -
                                   Left.Value.Value (1, 1) * Right.Value.Value (3, 1)
      ;
      Result.Value.Value (3, 1) := Left.Value.Value (1, 1) * Right.Value.Value (2, 1) -
                                   Left.Value.Value (2, 1) * Right.Value.Value (1, 1)
      ;

      return Result;
   end Cross;
end PragmARC.Matrix_Math;
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