-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Provides a postfix (reverse Polish notation) calculator with many features
--
-- History:
-- 2025 Jul 01     J. Carter          V2.1--Use SPDX license format
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
