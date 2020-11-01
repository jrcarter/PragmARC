-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Abstraction of a pool of job tasks
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jul 15     J. Carter          V1.1--Added Wait_Until_All_Idle
-- 2016 Jul 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with System;

generic -- PragmARC.Job_Pools
   type Job_Info is private;
   with procedure Process (Info : in Job_Info); -- Each task will call Process when it obtains a Job_Info

   Task_Priority : System.Priority := System.Default_Priority;

   with function Time_To_Quit return Boolean; -- Returns True when tasks should quit
   Quit_Check_Interval : Duration := Duration'Last; -- Idle tasks call Time_To_Quit at this interval

   Max_Tasks     : Positive := Integer'Last; -- Maximum number of job tasks that may be created
   Initial_Tasks : Natural  := 0;            -- Number of tasks to create during elaboration
package PragmARC.Job_Pools is
   pragma Assert (Initial_Tasks <= Max_Tasks, "Job_Pools: too many initial tasks");

   type Key_Handle is limited private;

   procedure Start_Job (Info : in Job_Info; Key : out Key_Handle);
   -- Makes Info available for processing
   -- Creates a new task if none are idle and there are fewer than Max_Tasks
   -- Jobs will be processed in the order that Start_Job is called
   -- Jobs may be processed out of order if Start_Job is called more than System.Max_Binary_Modulus times

   procedure Cancel (Key : in Key_Handle);
   -- Cancels Key if it is pending; no effect otherwise

   procedure Wait_Until_All_Idle;
   -- Blocks the caller until all tasks are idle and there are no pending jobs

   procedure Get_Statistics (Total : out Natural; Idle : out Natural; Pending : out Natural);
   -- Gives the number of total and idle tasks in use by the package, and number of pending jobs, at the time of the call
private -- PragmARC.Job_Pools
   type Key_Handle is mod System.Max_Binary_Modulus;
end PragmARC.Job_Pools;
