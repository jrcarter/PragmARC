-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2005 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Invokes a suspension object "safely" using the features of controlled types
--
-- History:
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
with Ada.Finalization;
with Ada.Synchronous_Task_Control;
package PragmARC.Safe_Suspension_Objects is
   type Safe_Suspension_Object (Unsafe : access Ada.Synchronous_Task_Control.Suspension_Object) is limited private;
   -- By declaring a Safe_Suspension_Object, the Suspension_Object designated by its Unsafe component
   -- will be used "safely"; that is, the features of controlled types will ensure that Suspend_Until_True
   -- will be called on Unsafe during the Safe_Suspension_Object's elaboration, and that Set_True will
   -- be called on Unsafe on exit from the Safe_Suspension_Object's scope, regardless of the path taken to that
   -- exit (including through an unhandled exception).
private -- PragmARC.Safe_Suspension_Objects
   type Safe_Suspension_Object (Unsafe : access Ada.Synchronous_Task_Control.Suspension_Object) is new
   Ada.Finalization.Limited_Controlled with record
      Needs_Finalization : Boolean := True;
   end record;
   
   procedure Initialize (Object : in out Safe_Suspension_Object);
   procedure Finalize   (Object : in out Safe_Suspension_Object);
end PragmARC.Safe_Suspension_Objects;
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
