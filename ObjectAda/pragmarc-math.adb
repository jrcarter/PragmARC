-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Functions for integers.
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2006 Mar 01     J. Carter          V1.0--Integer functions moved here
--
package body PragmARC.Math is
   function GCD (Left : in Natural; Right : in Natural) return Natural is
      Min       : Natural := Integer'Min (Left, Right);
      Max       : Natural := Integer'Max (Left, Right);
      Remainder : Natural;
   begin -- GCD
      Reduce : loop
         if Min = 0 then
            return Max;
         end if;

         Remainder := Max rem Min;
         Max := Min;
         Min := Remainder;
      end loop Reduce;
   end GCD;
end PragmARC.Math;
