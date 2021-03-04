-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.2--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.1--Added exception handler to Finalize
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Safety.Suspension_Objects is
   procedure Finalize (Object : in out Safe_Suspension_Object) is
      -- Empty
   begin -- Finalize
      if Object.Needs_Finalization then
         Ada.Synchronous_Task_Control.Set_True (Object.Unsafe.all);
         Object.Needs_Finalization := False;
      end if;
   exception -- Finalize
   when others =>
      null;
   end Finalize;

   procedure Initialize (Object : in out Safe_Suspension_Object) is
      -- Empty
   begin -- Initialize
      Ada.Synchronous_Task_Control.Suspend_Until_True (Object.Unsafe.all);
   end Initialize;
end PragmARC.Safety.Suspension_Objects;
