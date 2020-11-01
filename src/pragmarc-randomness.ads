-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Root of the Randomness hierarchy
--
-- Ada 95 and later define 2 standard random-number packages, Ada.Numerics.Float_Random & Ada.Numerics.Discrete_Random.
-- However, the random-number algorithm used by these packages is implementation defined. This package hierarchy provides portable
-- generators of known, good quality, should portability or the algorithm be a concern.
--
-- History:
-- 2020 Nov 01     J. Carter          V1.0--Initial Ada-12 version
--
package PragmARC.Randomness is
   pragma Pure;
end PragmARC.Randomness;
