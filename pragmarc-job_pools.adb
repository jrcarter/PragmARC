-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2016 Jul 15     J. Carter          V1.1--Added Wait_Until_All_Idle
-- 2016 Jul 01     J. Carter          V1.0--Initial version
--
with PragmARC.Skip_List_Unbounded;

package body PragmARC.Job_Pools is
   type Queue_Info is record
      Key : Key_Handle;
      Job : Job_Info;
   end record;

   function "<" (Left : Queue_Info; Right : Queue_Info) return Boolean;
   function "=" (Left : Queue_Info; Right : Queue_Info) return Boolean;

   package Job_Queues is new PragmARC.Skip_List_Unbounded (Element => Queue_Info);

   protected Job_Queue is
      procedure Put (Item : in Job_Info; Key : out Key_Handle);
      -- Adds Item to the queue.

      procedure Cancel (Key : in Key_Handle);
      -- Removes Key from the queue, if it exists

      entry Wait_Until_All_Idle;
      -- Blocks the caller until all tasks are idle and there are no pending jobs

      entry Get (Item : out Job_Info);
      -- Gets the oldest item in the queue in Item.
      -- If the queue is empty, blocks the caller until Put is called.

      function Waiting_Tasks return Natural;
      -- Returns the number of tasks blocked on Get.

      procedure Increment;
      -- Increments the task count, used to report the total number of tasks

      procedure Get_Statistics (Total : out Natural; Idle : out Natural; Pending : out Natural);
   private -- Job_Queue
      Queue : Job_Queues.Skip_List;
      Count : Natural := 0;
      Next  : Key_Handle := Key_Handle'First;
   end Job_Queue;

   task type Job_Task is
      pragma Priority (Task_Priority);
   end Job_Task;

   type Job_Task_Handle is access Job_Task;

   procedure Start_Job (Info : in Job_Info; Key : out Key_Handle) is
      pragma Warnings (Off);
      Local   : Job_Task_Handle;
      pragma Warnings (On);
      Total   : Natural;
      Idle    : Natural;
      Pending : Natural;
   begin -- Start_Job
      if Job_Queue.Waiting_Tasks <= 0 then
         Job_Queue.Get_Statistics (Total => Total, Idle => Idle, Pending => Pending);

         if Total < Max_Tasks then
            Local := new Job_Task;
            Job_Queue.Increment;
         end if;
      end if;

      Job_Queue.Put (Item => Info, Key => Key);
   end Start_Job;

   procedure Cancel (Key : in Key_Handle) is
      -- Empty
   begin -- Cancel
      Job_Queue.Cancel (Key => Key);
   end Cancel;

   procedure Wait_Until_All_Idle is
      -- Empty
   begin -- Wait_Until_All_Idle
      Job_Queue.Wait_Until_All_Idle;
   end Wait_Until_All_Idle;

   procedure Get_Statistics (Total : out Natural; Idle : out Natural; Pending : out Natural) is
      -- Empty
   begin -- Get_Statistics
      Job_Queue.Get_Statistics (Total => Total, Idle => Idle, Pending => Pending);
   end Get_Statistics;

   function "<" (Left : Queue_Info; Right : Queue_Info) return Boolean is
      -- Empty
   begin -- "<"
      return Left.Key < Right.Key;
   end "<";

   function "=" (Left : Queue_Info; Right : Queue_Info) return Boolean is
      -- Empty
   begin -- "="
      return Left.Key = Right.Key;
   end "=";

   protected body Job_Queue is
      procedure Put (Item : in Job_Info; Key : out Key_Handle) is
         -- Empty
      begin -- Put
         Key := Next;
         Next := Next + 1; -- If this wraps around, new jobs may be added before older ones
         Queue.Insert (Item => (Key => Key, Job => Item) );
      end Put;

      procedure Cancel (Key : in Key_Handle) is
         -- Empty
      begin -- Cancel
         Queue.Delete (Item => (Key => Key, Job => <>) );
      end Cancel;

      entry Wait_Until_All_Idle when Count = Get'Count and Queue.Length = 0 is
         -- Empty
      begin -- Wait_Until_All_Idle
         null;
      end Wait_Until_All_Idle;

      entry Get (Item : out Job_Info) when not Queue.Is_Empty is
         Queue_Item : Queue_Info;
      begin -- Get
         Queue_Item := Queue.Get_First;
         Queue.Delete (Item => Queue_Item);
         Item := Queue_Item.Job;
      end Get;

      function Waiting_Tasks return Natural is
         -- Empty
      begin -- Waiting_Tasks
         return Get'Count;
      end Waiting_Tasks;

      procedure Increment is
         -- Empty
      begin -- Increment
         Count := Count + 1;
      end Increment;

      procedure Get_Statistics (Total : out Natural; Idle : out Natural; Pending : out Natural) is
         -- Empty
      begin -- Get_Statistics
         Total := Count;
         Idle := Get'Count;
         Pending := Queue.Length;
      end Get_Statistics;
   end Job_Queue;

   task body Job_Task is
      Info : Job_Info;
   begin -- Job_Task
      Forever : loop
         select
            Job_Queue.Get (Item => Info);

            Handle_Error : begin
               Process (Info => Info);
            exception -- Handle_Error
            when others =>
               null;
            end Handle_Error;
         or
            delay Quit_Check_Interval;

            exit Forever when Time_To_Quit;
         end select;
      end loop Forever;
   end Job_Task;

   Dummy : Job_Task_Handle;
begin -- PragmARC.Job_Pools
   Create : for I in 1 .. Initial_Tasks loop
      Dummy := new Job_Task;
      Job_Queue.Increment;
   end loop Create;
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
