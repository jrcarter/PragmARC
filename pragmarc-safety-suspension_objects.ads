-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Invokes a suspension object "safely" using the features of controlled types
--
-- History:
-- 2025 Jul 01     J. Carter          V2.1--Use SPDX license format
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Synchronous_Task_Control;
private with Ada.Finalization;

package PragmARC.Safety.Suspension_Objects is
   type Suspension_Ptr is access all Ada.Synchronous_Task_Control.Suspension_Object;

   type Safe_Suspension_Object (Unsafe : Suspension_Ptr) is tagged limited private;
   -- By declaring a Safe_Suspension_Object, the Suspension_Object designated by its Unsafe component
   -- will be used "safely"; that is, the features of controlled types will ensure that Suspend_Until_True
   -- will be called on Unsafe during the Safe_Suspension_Object's elaboration, and that Set_True will
   -- be called on Unsafe on exit from the Safe_Suspension_Object's scope, regardless of the path taken to that
   -- exit (including through an unhandled exception).
private -- PragmARC.Safety.Suspension_Objects
   type Safe_Suspension_Object (Unsafe : Suspension_Ptr) is New Ada.Finalization.Limited_Controlled with record
      Needs_Finalization : Boolean := True;
   end record;

   procedure Initialize (Object : in out Safe_Suspension_Object);
   procedure Finalize   (Object : in out Safe_Suspension_Object);
end PragmARC.Safety.Suspension_Objects;
