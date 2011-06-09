-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Concurrent forwarder for task decoupling
--
-- History:
-- 2002 Oct 01     J. Carter          V1.2--Use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.1--Added Desired_Priority
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with System;
generic -- PragmARC.Forwarder
   type Element is limited private;

   with procedure Assign (To : out Element; From : in Element) is <>;

   with procedure Put (Item : in Element) is <>; -- Allows the Forwarder to Put the Element at the destination
   
   Desired_Priority : System.Any_Priority := System.Default_Priority; -- Priority for the task
package PragmARC.Forwarder is
   procedure Forward (Item : in Element);
   -- After returning from this call, the Forwarder calls Put with Item
   -- May block caller until previously forwarded item is delivered
private -- PragmARC.Forwarder
   pragma Inline (Forward);
end PragmARC.Forwarder;
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