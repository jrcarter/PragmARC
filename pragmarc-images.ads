-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Extended image functions for integer types
--
-- History:
-- 2006 Mar 01     J. Carter          V1.1--Added Float_Image
-- 2004 Apr 01     J. Carter          V1.0--Initial version
--
with Ada.Text_IO;
package PragmARC.Images is
   subtype Field       is Ada.Text_IO.Field;
   subtype Number_Base is Ada.Text_IO.Number_Base;

   -- The generic X_Image functions return an image of Value, at least Width characters wide, in base Base,
   -- padded with blanks or zeroes, according to Zero_Filled

   generic -- Signed_Image
      type Number is range <>;
   function Signed_Image (Value : Number; Width : Field := 0; Zero_Filled : Boolean := False; Base : Number_Base := 10)
   return String;

   generic -- Modular_Image
      type Number is mod <>;
   function Modular_Image (Value : Number; Width : Field := 0; Zero_Filled : Boolean := False; Base : Number_Base := 10)
   return String;

   generic -- Float_Image
      type Number is digits <>;
   function Float_Image
      (Value : Number; Fore : Field := 2; Aft : Field := Number'digits - 1; Exp : Field := 3; Zero_Filled : Boolean := False)
   return String;
   -- Returns an image of Value with at least Fore digits before the decimal point, padded with blanks or zeroes, according
   -- to Zero_Filled. Fore, Aft, and Exp have the same meanings as in Ada.Text_IO.Float_IO.
end PragmARC.Images;
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
