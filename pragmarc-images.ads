-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Extended image functions for integer and floating-point types
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Aug 15     J. Carter          V1.2--Added Base to Float_Image
-- 2006 Mar 01     J. Carter          V1.1--Added Float_Image
-- 2004 Apr 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Text_IO;

package PragmARC.Images is
   subtype Field       is Ada.Text_IO.Field;
   subtype Number_Base is Ada.Text_IO.Number_Base;

   -- The generic X_Image functions for integer types return an image of Value, at least Width characters wide, in base Base,
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
   function Float_Image (Value       : Number;
                         Fore        : Field       := 2;
                         Aft         : Field       := Number'Digits - 1;
                         Exp         : Field       := 3;
                         Zero_Filled : Boolean     := False;
                         Base        : Number_Base := 10)
   return String;
   -- If Base = 10, returns an image of Value with at least Fore digits before the decimal point, padded with blanks or zeroes,
   -- accordingto Zero_Filled. Fore, Aft, and Exp have the same meanings as in Ada.Text_IO.Float_IO
   -- If Base /= 10, Fore, Aft, and Exp are ignored, except that the result's length will be at least Fore + Aft + 1, and the image
   -- of Value in base Base will be as many digits as needed for the integer part, and no more than 1,000 digits for the fractional
   -- part
end PragmARC.Images;
