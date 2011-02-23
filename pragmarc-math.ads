-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides useful constants not defined by Ada.Numerics
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Numerics;

use Ada;
package PragmARC.Math is
   pragma Pure;

   -- Useful constants:
   Sqrt_2 : constant := 1.41421_35623_73095_04880_16887_24209_69807_85696_71875; -- Sqrt (2.0)
   Sqrt_3 : constant := 1.73205_08075_68877_29352_74463_41505_87236_69428_05254; -- Sqrt (3.0)
   Base_E : constant := Numerics.E;
   Log_2  : constant := 0.69314_71805_59945_30941_77321_21458_17656_80755_00134; -- Log ( 2.0)
   Log_10 : constant := 2.30258_50929_94045_68401_79914_54684_36420_76011_01488; -- Log (10.0)
   Pi     : constant := Numerics.Pi;
end PragmARC.Math;
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