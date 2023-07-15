-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides missing operations for converting vectors to and from their fixed equivalents
-- (equivalent to To_String and To_Unbounded_String for unbounded strings)
--
-- History
-- 2023 Jul 15     J. Carter          V2.2--Import "=" explicitly
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Jun 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Vectors;

generic -- PragmaRC.Conversions.Vectors
   type Index is range <>;
   type Element is private;
   type Fixed is array (Index range <>) of Element;

   with function "=" (Left : in Element; Right : in Element) return Boolean is <>;

   with package Unbounded is new Ada.Containers.Vectors (Index_Type => Index, Element_Type => Element);
package PragmARC.Conversions.Vectors is
   function To_Fixed (Vector : in Unbounded.Vector) return Fixed with
      Post => To_Fixed'Result'First = Index'First and then To_Fixed'Result'Last = Vector.Last_Index and then
              (for all I in To_Fixed'Result'Range => Vector.Element (I) = To_Fixed'Result (I) );

   function To_Vector (List : in Fixed) return Unbounded.Vector with
      Post => List'Length = To_Vector'Result.Last_Index and then
              (for all I in List'Range => List (I) = To_Vector'Result (I - List'First + Index'First) );
end PragmaRC.Conversions.Vectors;
