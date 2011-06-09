-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2006 Mar 01     J. Carter          V1.0--Initial release
--
with Ada.Numerics.Generic_Elementary_Functions;
package body PragmARC.Least_Squares_Fitting is
   procedure Line_Fit (Data : in Data_List; M : out Real; B : out Real; R_Sq : out Real) is
      N : constant Real := Real (Data'Length);
      
      Sum_X    : Real := 0.0;
      Sum_Y    : Real := 0.0;
      Sum_X_Sq : Real := 0.0;
      Sum_Y_Sq : Real := 0.0;
      Sum_XY   : Real := 0.0;
      
      package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
   begin -- Line_Fit
      All_Points : for I in Data'range loop
         Sum_X    := Sum_X + Data (I).X;
         Sum_Y    := Sum_Y + Data (I).Y;
         Sum_X_Sq := Sum_X_Sq + Data (I).X ** 2;
         Sum_Y_Sq := Sum_Y_Sq + Data (I).Y ** 2;
         Sum_XY   := Sum_XY + Data (I).X * Data (I).Y;
      end loop All_Points;

      M := (N * Sum_XY - Sum_X * Sum_Y) / (N * Sum_X_Sq - Sum_X ** 2);
      B := (Sum_Y * Sum_X_Sq - Sum_X * Sum_XY) / (N * Sum_X_Sq - Sum_X ** 2);
      R_Sq := (Sum_XY - Sum_X * Sum_Y / N) /
              Math.Sqrt ( (Sum_X_Sq - Sum_X ** 2 / N) * (Sum_Y_Sq - Sum_Y ** 2 / N) );
   end Line_Fit;
end PragmARC.Least_Squares_Fitting;
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
