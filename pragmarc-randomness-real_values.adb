-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************

-- History:
-- 2020 Nov 01     J. Carter     V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Oct 01     J. Carter     V1.1--Removed Random_Range and Normal, replaced by PragmARC.Real_Random_Ranges
-- 2013 Nov 01     J. Carter     V1.0--Initial release

package body PragmARC.Randomness.Real_Values is
   function Random (State : in out Generator) return Uniform is
      (Real (Unsigned_Random (State) ) / Real (Interfaces.Unsigned_32'Modulus) );
end PragmARC.Randomness.Real_Values;
