-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides complex numbers and operations
-- Ada.Numerics.Generic_Complex_Types provides all the functionality of this package
-- It is provided should your compiler not implement Annex G
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
generic -- PragmARC.Complex
   type Supplied_Real is digits <>;
package PragmARC.Complex is
   pragma Pure;

   subtype Real is Supplied_Real'Base;

   type Number is private;

   -- Unary operations
   function "+" (Right : Number) return Number;
   function "-" (Right : Number) return Number;

   -- Binary operations
   function "+" (Left : Number; Right : Number) return Number;
   function "-" (Left : Number; Right : Number) return Number;
   function "*" (Left : Number; Right : Number) return Number;
   function "/" (Left : Number; Right : Number) return Number;

   function "abs" (Right : Number) return Real; -- Also called magnitude or radius

   function "**" (Left : Number; Right : Integer) return Number;

   function Sqrt (Right : Number) return Number; -- Square root

   function Conjugate (Right : Number) return Number; -- Complex conjugate

   -- Conversions
   function To_Complex (Real_Part : Real; Imag_Part : Real) return Number;
   function Real_Part (Value : Number) return Real;
   function Imag_Part (Value : Number) return Real;
private -- PragmARC.Complex
   type Number is record
      Real_Part : Real := 0.0;
      Imag_Part : Real := 0.0;
   end record;
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