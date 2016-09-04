-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Sep 15     J. Carter          V1.0--Initial release
--
with Ada.Unchecked_Deallocation;

package body PragmARC.Holders is
   procedure Free is new Ada.Unchecked_Deallocation (Object => Element, Name => Element_Ptr);

   procedure Put (Onto : in out Handle; Item : in Element) is
      -- Empty
   begin -- Put
      Free (Onto.Ptr);
      Onto.Ptr := new Element'(Item);
   end Put;

   function Get (From : Handle) return Element is
      -- Empty
   begin -- Get
      if From.Ptr = null then -- Precondition
         raise Empty;
      end if;

      return From.Ptr.all;
   end Get;

   procedure Adjust (Object : in out Handle) is
      -- Empty
   begin -- Adjust
      Object.Ptr := new Element'(Object.Ptr.all);
   end Adjust;

   procedure Finalize (Object : in out Handle) is
      -- Empty
   begin -- Finalize
      Free (Object.Ptr);
   end Finalize;
end PragmARC.Holders;
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
