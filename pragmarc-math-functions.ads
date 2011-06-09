-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- This package is implemented in terms of Ada.Numerics.Generic_Elementary_Functions. It provides
-- argument checking via subtypes rather than using Argument_Error
--
-- History:
-- 2006 Mar 01     J. Carter          V1.2--Moved Integer functions to Integer_Functions
-- 2002 Oct 01     J. Carter          V1.1--Added GCD and LCM
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Math.Functions
   type Supplied_Real is digits <>;
package PragmARC.Math.Functions is
   pragma Pure;

   subtype Real is Supplied_Real'Base;

   subtype Natural_Real  is Real range  0.0              .. Real'Safe_Last;
   subtype Positive_Real is Real range  Real'Model_Small .. Real'Safe_Last;
   subtype Cosh_Real     is Real range  1.0              .. Real'Safe_Last;
   subtype Tanh_Real     is Real range -1.0              .. 1.0;

   Exp_Range_Violation : exception;
   -- The true acceptable range of values for Exp is Real'First .. Log (Positive_Real'Last),
   -- but there's no way to enforce that with a subtype
   -- This exception is raised if the argument (Right) to Exp is outside this range

   function "**" (Left : Natural_Real; Right : Real) return Real;

   function Sqrt (Right : Natural_Real) return Real; -- Positive square root

   function Exp (Right : Real) return Real; -- raise Exp_Range_Violation
   -- Returns Base_E ** Right
   --
   -- Precondition: Right in Real'First .. Log (Positive_Real'Last)     raises Exp_Range_Violation if violated

   function Log (Right : Positive_Real) return Real;
   -- Inverse of Exp; Log (Base_E) = 1.0 within the precision of the type
   -- Returns natural logarithm of Right

   Invalid_Angle : exception; -- Cot (0.0) and Coth (0.0) undefined

   -- Basic trig functions:
   function Sin (Angle : Real) return Real; -- Angles in radians
   function Cos (Angle : Real) return Real;
   function Tan (Angle : Real) return Real;
   function Cot (Angle : Real) return Real; -- raise Invalid_Angle
   -- Precondition: Angle /= 0.0    raises Invalid_Angle if violated

   -- Inverse trig functions:
   function Arcsin (Sin : Real) return Real; -- Results in radians
   function Arccos (Cos : Real) return Real;
   function Arctan (Tan : Real) return Real;
   function Arccot (Cot : Real) return Real;

   function Arctan (X : Real; Y : Real) return Real; -- X and Y are Cartesian coordinates; result is radians of polar equivalent

   -- Hyperbolic functions:
   function Sinh (Angle : Real) return Real; -- Angles in radians
   function Cosh (Angle : Real) return Real;
   function Tanh (Angle : Real) return Real;
   function Coth (Angle : Real) return Real; -- raise Invalid_Angle
   -- Precondition: Angle /= 0.0    raises Invalid_Angle if violated

   Invalid_Coth : exception; -- abs (argument to arccoth) must be > 1.0

   -- Inverse hyperbolic functions:
   function Arcsinh (Sinh : Real)      return Real; -- Results in radians
   function Arccosh (Cosh : Cosh_Real) return Real;
   function Arctanh (Tanh : Tanh_Real) return Real;
   function Arccoth (Coth : Real)      return Real; -- raise Invalid_Coth
   -- Precondition: abs Coth > 1.0      raises Invalid_Coth if violated
end PragmARC.Math.Functions;
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