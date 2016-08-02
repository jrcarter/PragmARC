-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Abstraction of a pool of job tasks
--
-- History:
-- 2016 Jul 15     J. Carter          V1.1--Added Wait_Until_All_Idle
-- 2016 Jul 01     J. Carter          V1.0--Initial version
--
with System;

generic -- PragmARC.Job_Pools
   type Job_Info is private;
   with procedure Process (Info : in Job_Info); -- Each task will call Process when it obtains a Job_Info

   Task_Priority : Standard.System.Priority := Standard.System.Default_Priority;

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
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
