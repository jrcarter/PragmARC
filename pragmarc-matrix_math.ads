-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Matrix manipulation routines
--
-- User supplies Element (number) type of matrices and constants and operations for the type
-- Must be able to supply negative one (-1) and zero (0) constants
-- The generic parameters are designed to make instantiation easy for a floating-point element:
--
--      use Ada.Numerics.Elementary_Functions; -- For Sqrt
--
--      package Real_Matrix is new Matrix_Math (Element => Float, Neg_One_Element => -1.0, Zero_Element => 0.0);
--
-- Type Element is private to allow more abstract types (complex numbers, for example)
--
-- History:
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Matrix_Math
   type Element is private; -- Element of a matrix

   Neg_One_Element : Element; -- -1.0
   Zero_Element    : Element; --  0.0

   with function "+"  (Left : Element; Right : Element) return Element is <>;
   with function "-"  (Left : Element; Right : Element) return Element is <>;
   with function "*"  (Left : Element; Right : Element) return Element is <>;
   with function "/"  (Left : Element; Right : Element) return Element is <>;
   with function "**" (Left : Element; Right : Integer) return Element is <>;
   with function "="  (Left : Element; Right : Element) return Boolean is <>;
   with function Sqrt (Num : Element)                   return Element is <>; -- Square root function for Norm
package PragmARC.Matrix_Math is
   pragma Pure;

   type Matrix_Representation is array (Positive range <>, Positive range <>) of Element;

   type Matrix (Num_Rows : Natural; Num_Columns : Natural) is record
      Value : Matrix_Representation (1 .. Num_Rows, 1 .. Num_Columns);
   end record;

   type Vector (Num_Elements : Natural) is record
      Value : Matrix (Num_Rows => Num_Elements, Num_Columns => 1);
   end record;

   Size_Mismatch : exception; -- The sizes of the matrices passed to an operation did not match for the operation
   Singular      : exception; -- Attempted to Invert a singular Matrix
   Not_Square    : exception; -- Attempted to Invert or find Determinant of a non-square Matrix
   Too_Small     : exception; -- Attempted to find Determinant of a null Matrix

   function "+" (Left : Matrix; Right : Matrix) return Matrix;
   function "-" (Left : Matrix; Right : Matrix) return Matrix;
   function "*" (Left : Matrix; Right : Matrix) return Matrix;
   function "/" (Left : Matrix; Right : Matrix) return Matrix; -- Uses Invert; Right must be a non-singular square Matrix

   function "*" (Left : Element; Right : Matrix)  return Matrix;
   function "*" (Left : Matrix;  Right : Element) return Matrix;
   function "/" (Left : Matrix;  Right : Element) return Matrix;

   function Transpose   (Mat : Matrix) return Matrix;
   function Invert      (Mat : Matrix) return Matrix;
   function Determinant (Mat : Matrix) return Element;
   function Cofactor    (Mat : Matrix; I : Positive; J : Positive) return Element;

   function Sub_Matrix (Mat : Matrix; From_Row : Positive; From_Col : Positive; To_Row : Positive; To_Col : Positive)
   return Matrix;
   -- Returns that part of Mat in rows From_Row .. To_Row and From_Col .. To_Col
   -- If From_Row > To_Row or From_Col > To_Col, a null Matrix will be returned

   function "+" (Left : Vector; Right : Vector) return Vector;
   function "-" (Left : Vector; Right : Vector) return Vector;
   function "*" (Left : Vector; Right : Vector) return Element; -- Dot product

   function "*" (Left : Element; Right : Vector)  return Vector;
   function "*" (Left : Vector;  Right : Element) return Vector;
   function "/" (Left : Vector;  Right : Element) return Vector;

   function "*" (Left : Matrix; Right : Vector) return Vector;
   function "*" (Left : Vector; Right : Matrix) return Matrix;

   function Transpose (Vec : Vector) return Matrix;  -- Returns a row Vector
   function Norm      (Vec : Vector) return Element; -- Norm or magnitude; Norm (V) = Sqrt (V * V)

   subtype Vector_3 is Vector (Num_Elements => 3);

   function Cross (Left : Vector_3; Right : Vector_3) return Vector_3;
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
