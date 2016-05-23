-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.2--Changed comment for empty declarative part and changed formatting
-- 2001 Dec 01     J. Carter          V1.1--Corrected context clause
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Numerics.Generic_Elementary_Functions;

use Ada;
use Ada.Numerics;
package body PragmARC.Complex is
   package Math is new Generic_Elementary_Functions (Float_Type => Real);

   function Sqrt (Right : Real) return Real renames Math.Sqrt;

   function "+" (Right : Number) return Number is
      -- Empty
   begin -- "+"
      return Right;
   end "+";

   function "-" (Right : Number) return Number is
      -- Empty
   begin -- "-"
      return Number'(Real_Part => -Right.Real_Part, Imag_Part => -Right.Imag_Part);
   end "-";

   function "+" (Left : Number; Right : Number) return Number is
      -- Empty
   begin -- "+"
      return Number'(Real_Part => Left.Real_Part + Right.Real_Part, Imag_Part => Left.Imag_Part + Right.Imag_Part);
   end "+";

   function "-" (Left : Number; Right : Number) return Number is
      -- Empty
   begin -- "-"
      return Number'(Real_Part => Left.Real_Part - Right.Real_Part, Imag_Part => Left.Imag_Part - Right.Imag_Part);
   end "-";

   function "*" (Left : Number; Right : Number) return Number is
      -- Empty
   begin -- "*"
      return Number'(Real_Part => Left.Real_Part * Right.Real_Part - Left.Imag_Part * Right.Imag_Part,
                     Imag_Part => Left.Imag_Part * Right.Real_Part + Left.Real_Part * Right.Imag_Part);
   end "*";

   function "/" (Left : Number; Right : Number) return Number is
      -- Empty
   begin -- "/"
      return Number'(Real_Part => (Left.Real_Part * Right.Real_Part + Left.Imag_Part * Right.Imag_Part) /
                                  (Right.Real_Part ** 2 + Right.Imag_Part ** 2),
                     Imag_Part => (Left.Imag_Part * Right.Real_Part - Left.Real_Part * Right.Imag_Part) /
                                  (Right.Real_Part ** 2 + Right.Imag_Part ** 2) );
   end "/";

   function "abs" (Right : Number) return Real is
      -- Empty
   begin -- "abs"
      return Sqrt (Right.Real_Part ** 2 + Right.Imag_Part ** 2);
   end "abs";

   function "**" (Left : Number; Right : Integer) return Number is
      Result : Number := Number'(Real_Part => 1.0, Imag_Part => 0.0);
   begin -- "**"
      Mult : for I in 1 .. abs Right loop
         Result := Left * Result;
      end loop Mult;

      if Right < 0 then
         return Number'(Real_Part => 1.0, Imag_Part => 0.0) / Result;
      else
         return Result;
      end if;
   end "**";

   function Sqrt (Right : Number) return Number is
      -- Empty
   begin -- Sqrt
      return Number'(Real_Part => Sqrt ( (abs Right + Right.Real_Part) / 2.0),
                     Imag_Part => Sqrt ( (abs Right - Right.Real_Part) / 2.0) );
   end Sqrt;

   function Conjugate (Right : Number) return Number is
      -- Empty
   begin -- Conjugate
      return Number'(Real_Part => Right.Real_Part, Imag_Part => -Right.Imag_Part);
   end Conjugate;

   function To_Complex (Real_Part : Real; Imag_Part : Real) return Number is
      -- Empty
   begin -- To_Complex
      return Number'(Real_Part => Real_Part, Imag_Part => Imag_Part);
   end To_Complex;

   function Real_Part (Value : Number) return Real is
      -- Empty
   begin -- Real_Part
      return Value.Real_Part;
   end Real_Part;

   function Imag_Part (Value : Number) return Real is
      -- Empty
   begin -- imag_part
      return Value.Imag_Part;
   end Imag_Part;
end PragmARC.Complex;
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
