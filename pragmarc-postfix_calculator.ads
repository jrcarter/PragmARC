-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides a postfix (reverse Polish notation) calculator with many features
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

package PragmARC.Postfix_Calculator is
   type Real is digits System.Max_Digits;

   function Calculator return Real;
   -- Runs a postfix calculator on Current_Input and Current_Output using Ansi_Tty_Control, returning the last value calculated
end PragmARC.Postfix_Calculator;
