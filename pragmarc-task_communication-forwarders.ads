-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Concurrent forwarder for task decoupling
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.2--Use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.1--Added Desired_Priority
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

generic -- PragmARC.Task_Communication.Forwarders
   type Element is private;

   with procedure Put (Item : in Element) is <>; -- Allows the Forwarder to Put the Element at the destination

   Desired_Priority : System.Any_Priority := System.Default_Priority; -- Priority for the task
package PragmARC.Task_Communication.Forwarders is
   procedure Forward (Item : in Element) with Inline;
   -- After returning from this call, the Forwarder calls Put with Item
   -- May block caller until previously forwarded item is delivered
end PragmARC.Task_Communication.Forwarders;
