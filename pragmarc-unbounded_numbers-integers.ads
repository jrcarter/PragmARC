-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Integers bounded only by Integer'Last and available memory
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2017 Apr 15     J. Carter          V1.1--Added GCD and LCM
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

private with Ada.Containers.Vectors;
private with System;

package PragmARC.Unbounded_Numbers.Integers is
   type Unbounded_Integer is private;
   -- Default initial value is zero

   subtype Convertible_Integer is Integer range -Integer'Last .. Integer'Last;

   function To_Unbounded_Integer (Value : in Convertible_Integer) return Unbounded_Integer;
   function "+" (Right : in Convertible_Integer) return Unbounded_Integer renames To_Unbounded_Integer;

   function To_Integer (Value : in Unbounded_Integer) return Integer;
   -- Raises Constraint_Error if Value is not convertible to Integer (which is usually the case if you need unbounded integers)
   function "+" (Right : in Unbounded_Integer) return Integer renames To_Integer;

   function "+" (Right : in Unbounded_Integer) return Unbounded_Integer;
   function "-" (Right : in Unbounded_Integer) return Unbounded_Integer;

   function "abs" (Right : in Unbounded_Integer) return Unbounded_Integer;

   function "+" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;
   function "-" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;
   function "*" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;
   function "/" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;

   function "rem" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;
   function "mod" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;

   function "**" (Left : in Unbounded_Integer; Right : in Natural) return Unbounded_Integer;

   function "="  (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Boolean;
   function ">"  (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Boolean;
   function "<"  (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Boolean;
   function ">=" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Boolean;
   function "<=" (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Boolean;

   type Base_Number is range 2 .. 36;

   function Image (Value : in Unbounded_Integer; Base : in Base_Number := 10; Decorated : in Boolean := False) return String;
   -- No initial blank for non-negative values
   -- If Decorated, the image includes the base in numeric-literal format: 16#FFFFFFFFFFFFFFFFFFFFFFFF#

   function Value (Image : in String) return Unbounded_Integer;
   -- Image must be equivalent to a result of calling function Image, possibly with leading or trailing spaces
   -- if Image is in a base other than 10, Image must be decorated
   -- Letters may be upper or lower case
   -- Raises Constraint_Error if Image is invalid

   function GCD (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;
   -- Greatest Common Divisor; since gcd(a,b)=gcd(|a|,|b|), signs are ignored
   -- Raises Constraint_Error if Left = Right = zero

   function LCM (Left : in Unbounded_Integer; Right : in Unbounded_Integer) return Unbounded_Integer;
   -- Least common multiple
private -- PragmARC.Unbounded_Numbers.Integers
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
end PragmARC.Unbounded_Numbers.Integers;
