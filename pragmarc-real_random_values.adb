-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2013 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************

-- History:
-- 2013 Nov 01     J. Carter     v1.0--Initial release

package body PragmARC.Real_Random_Values is
   function Random (State : in Generator) return Real is
      -- Empty declarative part
   begin -- Random
      return Real (Unsigned_Random (State) ) / Real (Interfaces.Unsigned_32'Modulus);
   end Random;

   function Random_Range (State : in Generator; Min : in Real; Max : in Real) return Real is
      -- Empty declarative part
   begin -- Random_Range
      return Random (State) * (Max - Min) + Min;
   end Random_Range;

   function Normal (State : in Generator; Mean : in Real; Sigma : in Real) return Real is
      Sum : Real := 0.0;
   begin -- Normal
      Add : for I in 1 .. 12 loop
         Sum := Sum + Random (State);
      end loop Add;

      return Sigma * (Sum - 6.0) + Mean;
   end Normal;
end PragmARC.Real_Random_Values;
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
