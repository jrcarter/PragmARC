-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Binary semaphore for controlling concurrent access not suitable for a concurrent form or a monitor
--
-- History:
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2001 Dec 01     J. Carter          V1.1--Added Ceiling_Priority to type
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

package PragmARC.Binary_Semaphore_Handler with Pure is
   protected type Binary_Semaphore (Ceiling_Priority : System.Any_Priority := System.Default_Priority) with
      Priority => Ceiling_Priority
   is
      entry Request; -- Obtain semaphore; may block caller indefinitely

      entry Release;
      -- Release semaphore so another caller may obtain it
      -- May block caller indefinitely if semaphore has been used incorrectly
   private -- Binary_Semaphore
      In_Use : Boolean := False;
   end Binary_Semaphore;
   -- Initially ready to be obtained
   -- User must ensure that a Binary_Semaphore is used correctly:
   -- Each user calls Request before calling Release
   -- Each user calls Release after calling Request
end PragmARC.Binary_Semaphore_Handler;
