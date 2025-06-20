-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2025 Jul 01     J. Carter          V2.1--Use SPDX license format
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
