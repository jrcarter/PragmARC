-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2006 Mar 01     J. Carter          V1.2--Moved Integer functions to Integer_Functions
-- 2002 Oct 01     J. Carter          V1.1--Added GCD and LCM
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Numerics.Generic_Elementary_Functions;

use Ada;
package body PragmARC.Math.Functions is
   package Mth is new Numerics.Generic_Elementary_Functions (Float_Type => Real);

   function "**" (Left : Natural_Real; Right : Real) return Real is
      -- null;
   begin -- "**"
      return Mth."**" (Left, Right);
   end "**";

   function Sqrt (Right : Natural_Real) return Real is
      -- null;
   begin -- Sqrt
      return Mth.Sqrt (Right);
   end Sqrt;

   function Exp (Right : Real) return Real is
      -- null;
   begin -- Exp
      return Mth.Exp (Right);
   exception -- Exp
   when others =>
      raise Exp_Range_Violation;
   end Exp;

   function Log (Right : Positive_Real) return Real is
      -- null;
   begin -- Log
      return Mth.Log (Right);
   end Log;

   function Sin (Angle : Real) return Real is
      -- null;
   begin -- Sin
      return Mth.Sin (Angle);
   end Sin;

   function Cos (Angle : Real) return Real is
      -- null;
   begin -- Cos
      return Mth.Cos (Angle);
   end Cos;

   function Tan (Angle : Real) return Real is
      -- null;
   begin -- Tan
      return Mth.Tan (Angle);
   end Tan;

   function Cot (Angle : Real) return Real is
      -- null;
   begin -- Cot
      return Mth.Cot (Angle);
   exception -- Cot
   when others =>
      raise Invalid_Angle;
   end Cot;

   function Arcsin (Sin : Real) return Real is
      -- null;
   begin -- Arcsin
      return Mth.Arcsin (Sin);
   end Arcsin;

   function Arccos (Cos : Real) return Real is
      -- null;
   begin -- Arccos
      return Mth.Arccos (Cos);
   end Arccos;

   function Arctan (Tan : Real) return Real is
      -- null;
   begin -- Arctan
      return Mth.Arctan (Tan);
   end Arctan;

   function Arccot (Cot : Real) return Real is
      -- null;
   begin -- arccot
      return Mth.Arccot (Cot);
   end Arccot;

   function Arctan (X : Real; Y : Real) return Real is
      -- null;
   begin -- Arctan
      return Mth.Arctan (Y, X);
   end Arctan;

   function Sinh (Angle : Real) return Real is
      -- null;
   begin -- Sinh
      return Mth.Sinh (Angle);
   end Sinh;

   function Cosh (Angle : Real) return Real is
      -- null;
   begin -- Cosh
      return Mth.Cosh (Angle);
   end Cosh;

   function Tanh (Angle : Real) return Real is
      -- null;
   begin -- Tanh
      return Mth.Tanh (Angle);
   end Tanh;

   function Coth (Angle : Real) return Real is
      -- null;
   begin -- Coth
      return Mth.Coth (Angle);
   exception -- Coth
   when others =>
      raise Invalid_Angle;
   end Coth;

   function Arcsinh (Sinh : Real) return Real is
      -- null;
   begin -- Arcsinh
      return Mth.Arcsinh (Sinh);
   end Arcsinh;

   function Arccosh (Cosh : Cosh_Real) return Real is
      -- null;
   begin -- Arccosh
      return Mth.Arccosh (Cosh);
   end Arccosh;

   function Arctanh (Tanh : Tanh_Real) return Real is
      -- null;
   begin -- Arctanh
      return Mth.Arctanh (Tanh);
   end Arctanh;

   function Arccoth (Coth : Real) return Real is
      -- null;
   begin -- Arccoth
      return Mth.Arccoth (Coth);
   exception -- Arccoth
   when others =>
      raise Invalid_Coth;
   end Arccoth;
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