-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V1.2--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.1--Added exception handler to Finalize
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Safe_Suspension_Objects is
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

