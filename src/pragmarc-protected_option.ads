-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides a Boolean Enabled/Disabled option selection suitable for concurrent use
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2001 Dec 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

package PragmARC.Protected_Option is
   protected type Handle (Initially_Enabled : Boolean := False; Ceiling_Priority : System.Any_Priority := System.Default_Priority)
      with Priority => Ceiling_Priority
   is
      function Enabled return Boolean;
      -- Returns the state of this option

      procedure Enable (Enabled : in Boolean := True);
      -- Set the state of this option to Enabled
   private -- Handle
      State : Boolean := Initially_Enabled;
   end Handle ;
end PragmARC.Protected_Option;
