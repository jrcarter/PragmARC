-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generate permutations of a sequence
--
-- History:
-- 2023 Nov 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Indefinite_Vectors;

generic -- PragmARC.Permutations
   type Element is private;
package PragmARC.Permutations is
   type Sequence is array (Positive range <>) of Element;

   procedure Generate (Initial : in Sequence; Process : access procedure (Seq : in Sequence; Stop : in out Boolean) ) with
      Pre => Initial'First = 1;
   -- Generates all PragmARC.Permutations of Initial and passes them to Process with Stop => False
   -- Stops immediately if Process sets Stop to True

   package Sequence_Lists is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => Sequence);

   procedure Generate (Initial : in Sequence; Result : in out Sequence_Lists.Vector) with
      Pre => Initial'First = 1;
   -- Clears Result, then generates all PragmARC.Permutations of Initial and stores them in Result
   -- Note that there will be Initial'Length! PragmARC.Permutations
end PragmARC.Permutations;
