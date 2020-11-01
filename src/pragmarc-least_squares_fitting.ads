-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2006 Mar 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Least_Squares_Fitting
   type Supplied_Real is digits <>;
package PragmARC.Least_Squares_Fitting is
   subtype Real is Supplied_Real'Base;

   type Point is record
      X : Real;
      Y : Real;
   end record;

   type Data_List is array (Positive range <>) of Point;

   procedure Line_Fit (Data : in Data_List; M : out Real; B : out Real; R_Sq : out Real) with
      Pre => Data'Length > 1;
   -- Least Squares fit of a line to Data. M is the slope, B the Y-intercept,
   -- and R_Sq the squared correlation coeffiecient.
end PragmARC.Least_Squares_Fitting;
