-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Monitor for concurrent use
--
-- History:
-- 2025 Jul 01     J. Carter          V2.1--Use SPDX license format
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
-------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.2--Use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.1--Added Ceiling_Priority to Monitor
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

generic -- PragmARC.Task_Communication.Monitors
   type Element is private;
package PragmARC.Task_Communication.Monitors with Pure is
   protected type Monitor (Ceiling_Priority : System.Any_Priority := System.Default_Priority) with
      Priority => Ceiling_Priority
   is
      procedure Put (Item : in Element); -- Change the value stored in the monitor

      function Get return Element; -- Obtain the value stored in the monitor
   private -- Monitor
      Value : Element;
   end Monitor;
end PragmARC.Task_Communication.Monitors;
