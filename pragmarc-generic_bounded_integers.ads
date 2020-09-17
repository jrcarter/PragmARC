-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Integers bounded by a Max_Binary_Digits.
-- Daniel Original Motive: For create Databases and Distributed Systems where a
-- 64-bit integers are not enough and For creating a Scaled_Integer Float
-- package for use with money and financial systems.
--
-- History:
-- 2020 Sep 17     Daniel N. Moraes    V1.0--Initial release

private with Ada.Containers.Bounded_Vectors;
private with System;

generic
   Max_Binary_Digits : Positive;
package PragmARC.Generic_Bounded_Integers

is
   type Bounded_Integer is private;
   -- Default initial value is zero

   function To_Bounded_Integer (Value : Integer) return Bounded_Integer;
   -- With some compilers Integer'First will cause Constraint_Error
   function "+" (Right : Integer) return Bounded_Integer renames To_Bounded_Integer;

   function To_Integer (Value : Bounded_Integer) return Integer;
   -- Raises Constraint_Error if Value is not convertible to Integer (which is usually the case if you need unbounded integers)
   function "+" (Right : Bounded_Integer) return Integer renames To_Integer;

   function "+" (Right : Bounded_Integer) return Bounded_Integer;
   function "-" (Right : Bounded_Integer) return Bounded_Integer;

   function "abs" (Right : Bounded_Integer) return Bounded_Integer;

   function "+" (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;
   function "-" (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;
   function "*" (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;
   function "/" (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;

   function "rem" (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;
   function "mod" (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;

   function "**" (Left : Bounded_Integer; Right : Natural) return Bounded_Integer;

   function "="  (Left : Bounded_Integer; Right : Bounded_Integer) return Boolean;
   function ">"  (Left : Bounded_Integer; Right : Bounded_Integer) return Boolean;
   function "<"  (Left : Bounded_Integer; Right : Bounded_Integer) return Boolean;
   function ">=" (Left : Bounded_Integer; Right : Bounded_Integer) return Boolean;
   function "<=" (Left : Bounded_Integer; Right : Bounded_Integer) return Boolean;

   type Base_Number is range 2 .. 36;

   function Image (Value : Bounded_Integer; Base : Base_Number := 10; Decorated : Boolean := False) return String;
   -- No initial blank for non-negative values
   -- If Decorated, the image includes the base in numeric-literal format: 16#FFFFFFFFFFFFFFFFFFFFFFFF#

   function Value (Image : String) return Bounded_Integer;
   -- Image must be equivalent to a result of calling function Image, possibly with leading or trailing spaces
   -- if Image is in a base other than 10, Image must be decorated
   -- Letters may be upper or lower case
   -- Raises Constraint_Error if Image is invalid

   function GCD (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;
   -- Greatest Common Divisor; since gcd(a,b)=gcd(|a|,|b|), signs are ignored
   -- Raises Constraint_Error if Left = Right = zero

   function LCM (Left : Bounded_Integer; Right : Bounded_Integer) return Bounded_Integer;
   -- Least common multiple

   function Binary_Digits return positive;

   function Decimal_Digits return positive;

private -- PragmARC.Bounded_Integers
   use type Ada.Containers.Count_Type;

   type Calculation_Value is mod System.Max_Binary_Modulus;

   Digit_Size : constant := Calculation_Value'Size / 2;
   
   Max_Capacity : constant Positive := Positive ( Long_Long_Float'Ceiling
      (Long_Long_Float (Max_Binary_Digits) / Long_Long_Float (Digit_Size)));

   type Digit_Value is mod 2 ** Digit_Size
      with Default_Value => 0;

   package Digit_Lists is new Ada.Containers.Bounded_Vectors (Index_Type => Positive, Element_Type => Digit_Value);

   subtype Digit_List is Digit_Lists.Vector(Ada.Containers.Count_Type(Max_Capacity));

   function Single_Zero return Digit_List;
   -- Returns a list of one digit, which is zero

   type Bounded_Integer is record
      Negative : Boolean    := False;
      Digit    : Digit_List := Single_Zero;
   end record;
end PragmARC.Generic_Bounded_Integers;
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
