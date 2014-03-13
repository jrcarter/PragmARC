-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Integers bounded only by Integer'Last and available memory
--
-- History:
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
private with Ada.Containers.Vectors;
private with System;

package PragmARC.Unbounded_Integers is
   type Unbounded_Integer is private;
   -- Default initial value is zero

   function To_Unbounded_Integer (Value : Integer) return Unbounded_Integer;
   -- With some compilers Integer'First will cause Constraint_Error

   function To_Integer (Value : Unbounded_Integer) return Integer;
   -- Raises Constraint_Error if Value is not convertible to Integer (which is usually the case if you need unbounded integers)

   function "+" (Right : Unbounded_Integer) return Unbounded_Integer;
   function "-" (Right : Unbounded_Integer) return Unbounded_Integer;

   function "abs" (Right : Unbounded_Integer) return Unbounded_Integer;

   function "+" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;
   function "-" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;
   function "*" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;
   function "/" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;

   function "rem" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;
   function "mod" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;

   function "**" (Left : Unbounded_Integer; Right : Natural) return Unbounded_Integer;

   function "="  (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean;
   function ">"  (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean;
   function "<"  (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean;
   function ">=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean;
   function "<=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean;

   type Base_Number is range 2 .. 36;

   function Image (Value : Unbounded_Integer; Base : Base_Number := 10; Decorated : Boolean := False) return String;
   -- No initial blank for non-negative values
   -- If Decorated, the image includes the base in numeric-literal format: 16#FFFFFFFFFFFFFFFFFFFFFFFF#

   function Value (Image : String) return Unbounded_Integer;
   -- Image must be equivalent to a result of calling function Image, possibly with leading or trailing spaces
   -- if Image is in a base other than 10, Image must be decorated
   -- Letters may be upper or lower case
   -- Raises Constraint_Error if Image is invalid
private -- PragmARC.Unbounded_Integers
   type Calculation_Value is mod System.Max_Binary_Modulus;

   Digit_Size : constant := Calculation_Value'Size / 2;

   type Digit_Value is mod 2 ** Digit_Size;

   package Digit_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Digit_Value);

   subtype Digit_List is Digit_Lists.Vector;

   function Single_Zero return Digit_List;
   -- Returns a list of one digit, which is zero

   type Unbounded_Integer is record
      Negative : Boolean    := False;
      Digit    : Digit_List := Single_Zero;
   end record;
end PragmARC.Unbounded_Integers;
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
