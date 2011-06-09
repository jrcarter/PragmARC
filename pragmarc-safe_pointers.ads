-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2005 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides "safe" pointers; pointers that deallocate themselves automatically
-- when they go out of scope and the designated object has no more references.
--
-- History:
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
with Ada.Finalization;
generic -- PragmARC.Safe_Pointers
   type Object is private;
package PragmARC.Safe_Pointers is
   type Safe_Pointer is private;
   
   Null_Pointer : constant Safe_Pointer;
   
   function Allocate return Safe_Pointer;
   -- Equivalent to "new Object".
   -- May raise Storage_Error.
   
   function Allocate (Data : Object) return Safe_Pointer;
   -- Equivalent to "new Object'(Data)".
   -- May raise Storage_Error.
   
   function Get (Pointer : Safe_Pointer) return Object;
   -- Equivalent to "Pointer.all".
   -- May raise Constraint_Error.
   --
   -- Precondition: Pointer /= Null_Pointer     raises Constraint_Error if violated
   
   procedure Put (Pointer : in Safe_Pointer; Value : in Object);
   -- Equivalent to "Pointer.all := Value;".
   -- May raise Constraint_Error.
   --
   -- Precondition:  Pointer /= Null_Pointer     raises Constraint_Error if violated
   -- Postcondition: Get (Pointer) = Value
   
   function "=" (Left : Safe_Pointer; Right : Safe_Pointer) return Boolean;
   -- Returns True if Left and Right have the same access value; False otherwise.
private -- PragmARC.Safe_Pointers
   type Safe_Group;
   
   type Name is access Safe_Group;
   
   type Safe_Pointer is new Ada.Finalization.Controlled with record
      Ptr : Name;
   end record;
   
   procedure Adjust   (Item : in out Safe_Pointer);
   procedure Finalize (Item : in out Safe_Pointer);
   
   Null_Pointer : constant Safe_Pointer := Safe_Pointer'(Ada.Finalization.Controlled with Ptr => null);
end PragmARC.Safe_Pointers;
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
