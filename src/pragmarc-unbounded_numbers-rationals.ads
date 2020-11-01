-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Rational numbers bounded only by Integer'Last and available memory
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Aug 15     J. Carter          V1.3--Apply Base to non-fractional images; improve Sqrt convergence
-- 2017 Apr 15     J. Carter          V1.2--Added Sqrt and improved "**"
-- 2014 Jun 01     J. Carter          V1.1--Improved Image
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Unbounded_Numbers.Integers;

package PragmARC.Unbounded_Numbers.Rationals is
   type Rational is private;
   -- Default initial value is zero

   Zero : constant Rational;
   One  : constant Rational;

   function Compose (Numerator : Integers.Unbounded_Integer; Denominator : Integers.Unbounded_Integer) return Rational;
   -- Creates the value Numerator / Denominator
   function "/" (Left : Integers.Unbounded_Integer; Right : Integers.Unbounded_Integer) return Rational renames Compose;

   procedure Decompose
      (Value : in Rational; Numerator : out Integers.Unbounded_Integer; Denominator : out Integers.Unbounded_Integer);
   -- Returns the Numerator and Denominator that make up Value
   -- The sign of Value will be in Numerator; Denominator will be positive

   function "+" (Right : Rational) return Rational;
   function "-" (Right : Rational) return Rational;

   function "abs" (Right : Rational) return Rational;

   function "+" (Left : Rational; Right : Rational) return Rational;
   function "-" (Left : Rational; Right : Rational) return Rational;
   function "*" (Left : Rational; Right : Rational) return Rational;
   function "/" (Left : Rational; Right : Rational) return Rational;

   function "**" (Left : Rational; Right : Integer) return Rational;

   function ">"  (Left : Rational; Right : Rational) return Boolean;
   function "<"  (Left : Rational; Right : Rational) return Boolean;
   function ">=" (Left : Rational; Right : Rational) return Boolean;
   function "<=" (Left : Rational; Right : Rational) return Boolean;

   type Base_Number is range 2 .. 36;

   function Image (Value : Rational; As_Fraction : Boolean := False; Base : Base_Number := 10; Decorated : Boolean := False)
   return String;
   -- Returns the image of Value
   -- If As_Fraction, result is in the format Numerator/Denominator; otherwise, result is in the format of a real literal
   -- No initial blank for non-negative values
   -- If Decorated, the image includes the base in numeric-literal format: 16#FFFFFFFFFFFFFFFFFFFFFFFF#/16#7#, 2#1.1#
   -- If not As_Fraction
   --    if abs Value < 1.0, result will have at most 1,000 significant digits
   --    otherwise, result will have at most 1,000 digits to the right of the radix point

   function Value (Image : String) return Rational;
   -- Image must be one of:
   --    Equivalent to a result of calling function Image, possibly with leading or trailing spaces
   --    The image of an integer literal (9876543210)
   --    The image of a real literal (9.87654321)
   -- if Image is in a base other than 10, Image must be decorated
   -- Letters may be upper or lower case
   -- Raises Constraint_Error if Image is invalid

   function Sqrt (Right : Rational; Accuracy : Rational) return Rational with
      Pre => Right >= Zero or else raise Constraint_Error;
   -- Square root of non-negative value
   -- Square of result will be within abs Accuracy of Right
private -- PragmARC.Unbounded_Numbers.Rationals
   use PragmARC.Unbounded_Numbers.Integers;

   UI0 : constant Unbounded_Integer := To_Unbounded_Integer (0);
   UI1 : constant Unbounded_Integer := To_Unbounded_Integer (1);

   type Rational is record
      Numerator   : Unbounded_Integer := UI0; -- Sign in Numerator
      Denominator : Unbounded_Integer := UI1;
   end record;

   Zero : constant Rational := (others => <>);
   One  : constant Rational := (Numerator => UI1, Denominator => UI1);
end PragmARC.Unbounded_Numbers.Rationals;
