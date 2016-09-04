-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides holders for values of indefinite types
--
-- History:
-- 2016 Sep 15     J. Carter          V1.0--Initial release
--
private with Ada.Finalization;

generic -- PragmARC.Holders
   type Element (<>) is private;
package PragmARC.Holders is
   pragma Preelaborate;

   type Handle is tagged private;
   -- Initial value: empty

   procedure Put (Onto : in out Handle; Item : in Element);
   -- Makes the value stored in Onto be Item
   -- Onto is not empty after a call to Put

   function Get (From : Handle) return Element;
   -- Returns the value stored in From
   -- From must not be empty; raises Empty if it is
   --
   -- Precondition: From is not empty     raise Empty if violated
private -- PragmARC.Holders
   type Element_Ptr is access Element;

   type Handle is new Ada.Finalization.Controlled with record
      Ptr : Element_Ptr;
   end record;

   procedure Adjust (Object : in out Handle);
   procedure Finalize (Object : in out Handle);
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
