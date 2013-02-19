-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2002 Oct 01     J. Carter          V1.2--Use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.1--Added Desired_Priority
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Forwarder is
   task Handler is
      pragma Priority (Desired_Priority);
      
      entry Forward (Item : in Element);
   end Handler;

   procedure Forward (Item : in Element) is
      -- null;
   begin -- Forward
      Handler.Forward (Item => Item);
   end Forward;

   task body Handler is
      Store : Element;
   begin -- Handler
      Forever : loop
         select
            accept Forward (Item : in Element) do
               Assign (To => Store, From => Item);
            end Forward;
         or
            terminate;
         end select;

         Put (Item => Store);
      end loop Forever;
   end Handler;
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