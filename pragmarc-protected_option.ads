-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides a Boolean Enabled/Disabled option selection suitable for concurrent use
--
-- History:
-- 2001 Dec 01     J. Carter          V1.0--Initial release
--
with System;
package PragmARC.Protected_Option is
   protected type Handle (Initially_Enabled : Boolean := False; Ceiling_Priority : System.Any_Priority := System.Default_Priority)
   is
      pragma Priority (Ceiling_Priority);
      
      function Enabled return Boolean;
      -- Returns the state of this option
      
      procedure Enable (Enabled : in Boolean := True);
      -- Set the state of this option to Enabled
   private -- Handle
      State : Boolean := Initially_Enabled;
   end Handle ;
end PragmARC.Protected_Option;
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