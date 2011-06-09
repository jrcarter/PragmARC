-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Transporter task for task decoupling
--
-- History:
-- 2001 Dec 01     J. Carter          V1.1--Added Desired_Priority
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with System;
generic -- PragmARC.Transporter_Handler
   type Data_To_Transport (<>) is limited private;

   with function Get return Data_To_Transport;
   with procedure Put (Data : in Data_To_Transport);
package PragmARC.Transporter_Handler is
   pragma Pure;

   task type Transporter (Desired_Priority : System.Any_Priority := System.Default_Priority) is
      pragma Priority (Desired_Priority);
   end Transporter;

   -- A Transporter is a task with a body of the form:
   -- Forever : loop
   --    Put (Data => Get);
   -- end loop Forever;
   --
   -- Since it's impossible for the task to time out on a call to Get or Put, the only way to stop it is abort
   -- Hopefully, most systems which need a Transporter will run non-stop, so this will not be needed
end PragmARC.Transporter_Handler;
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