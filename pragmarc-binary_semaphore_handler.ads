-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Binary semaphore for controlling concurrent access not suitable for a concurrent form or a monitor
--
-- History:
-- 2001 Dec 01     J. Carter          V1.1--Added Ceiling_Priority to type
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with System;
package PragmARC.Binary_Semaphore_Handler is
   pragma Pure;

   protected type Binary_Semaphore (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is
      pragma Priority (Ceiling_Priority);
      
      entry Request; -- Obtain semaphore; may block caller indefinitely

      entry Release;
      -- Release semaphore so another caller may obtain it
      -- May block caller indefinitely if semaphore has been used incorrectly
   private -- Binary_Semaphore
      In_Use : Boolean := False;
   end Binary_Semaphore;
   -- Initially ready to be obtained
   -- User must ensure that a Binary_Semaphore is used correctly:
   -- Each user calls Request before calling Release
   -- Each user calls Release after calling Request
end PragmARC.Binary_Semaphore_Handler;
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