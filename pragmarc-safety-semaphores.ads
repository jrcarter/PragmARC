-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Invokes a binary semaphore "safely" using the features of controlled types
--
-- History:
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Binary_Semaphore_Handler;
private with Ada.Finalization;

package PragmARC.Safety.Semaphores with Preelaborate is
   type Semaphore_Ptr is access all Binary_Semaphore_Handler.Binary_Semaphore;

   type Safe_Semaphore (Unsafe : Semaphore_Ptr) is tagged limited private;
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
private -- PragmARC.Safety.Semaphores
   type Safe_Semaphore (Unsafe : Semaphore_Ptr) is new Ada.Finalization.Limited_Controlled with record
      Needs_Finalization : Boolean := True;
   end record;

   procedure Initialize (Object : in out Safe_Semaphore);
   procedure Finalize   (Object : in out Safe_Semaphore);
end PragmARC.Safety.Semaphores;
