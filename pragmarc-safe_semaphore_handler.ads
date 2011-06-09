-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Invokes a binary semaphore "safely" using the features of controlled types
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Binary_Semaphore_Handler;
with Ada.Finalization;

use Ada;
package PragmARC.Safe_Semaphore_Handler is
   pragma Preelaborate;

   type Safe_Semaphore (Unsafe : access Binary_Semaphore_Handler.Binary_Semaphore) is limited private;
   -- By declaring a Safe_Semaphore, the Binary_Semaphore designated by its Unsafe component
   -- will be used "safely"; that is, the features of controlled types will ensure that Unsafe's
   -- Request entry will be called during the Safe_Semaphore's elaboration, and that Unsafe's Release
   -- entry is called on exit from the Safe_Semaphore's scope, regardless of the path taken to that
   -- exit (including through an unhandled exception).
   --
   -- Generally, Binary_Semaphores are unsafe because
   -- 1. The Request entry call may be forgotten
   -- 2. A path into the critical region may bypass the Request call
   -- 3. The Release entry call may be forgotten
   -- 4. A path out of the critical region may bypass the Release call (especially if exceptions
   --    are involved)
   --
   -- With a Safe_Semaphore, this list is reduced to
   -- 1. The declaration of the Safe_Semaphore may be omitted
   -- 2. Unsafe may designate the wrong semaphore
   --
   -- Example of use:
   -- S : aliased Binary_Semaphore;
   -- ...
   -- Critical_Region : declare
   --    Safe : Safe_Semaphore (Unsafe => S'access); -- S.Request called here
   -- begin -- Critical_Region
   --    ...
   -- end Critical_Region; -- S.Release called on exit from Critical_Region
private -- PragmARC.Safe_Semaphore_Handler
   type Safe_Semaphore (Unsafe : access Binary_Semaphore_Handler.Binary_Semaphore) is new Finalization.Limited_Controlled
   with record
      Needs_Finalization : Boolean := True;
   end record;

   procedure Initialize (Object : in out Safe_Semaphore);
   procedure Finalize   (Object : in out Safe_Semaphore);
end PragmARC.Safe_Semaphore_Handler;
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