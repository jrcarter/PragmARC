-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Transporter task for task decoupling
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2001 Dec 01     J. Carter          V1.1--Added Desired_Priority
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

generic -- PragmARC.Task_Communication.Transporters
   type Data_To_Transport (<>) is limited private;

   with function Get return Data_To_Transport;
   with procedure Put (Data : in Data_To_Transport);
package PragmARC.Task_Communication.Transporters with Pure is
   task type Transporter (Desired_Priority : System.Any_Priority := System.Default_Priority) is
      pragma Priority (Desired_Priority);
   end Transporter;

   -- A Transporter is a task with a body of the form:
   -- Forever : loop
   --    Put (Data => Get);
   -- end loop Forever;
   --
   -- Since it's impossible for the task to time out on a call to Get or Put, the only way to stop it is abort
   -- Hopefully, most systems which need a Transporter will run non-stop, so this will not be needed
end PragmARC.Task_Communication.Transporters;
