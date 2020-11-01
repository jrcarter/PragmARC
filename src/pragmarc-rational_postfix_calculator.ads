-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides a postfix (reverse Polish notation) calculator capable of handling very small and very large values
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2017 Apr 15     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Unbounded_Numbers.Rationals;

function PragmARC.Rational_Postfix_Calculator return PragmARC.Unbounded_Numbers.Rationals.Rational;
-- Runs a postfix calculator on Current_Input and Current_Output using Ansi_Tty_Control, returning the last value calculated
