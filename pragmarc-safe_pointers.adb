-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2005 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2011 Jul 01     J. Carter          V1.1--Finalize may be called multiple times
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
with Ada.Unchecked_Deallocation;
package body PragmARC.Safe_Pointers is
   type Safe_Group is record
      Data  : Object;
      Count : Natural := 1;
   end record;

   function "=" (Left : Safe_Pointer; Right : Safe_Pointer) return Boolean is
      -- null;
   begin -- "="
      return Left.Ptr = Right.Ptr;
   end "=";

   procedure Adjust (Item : in out Safe_Pointer) is
      -- null;
   begin -- Adjust
      if Item.Ptr /= null then
         Item.Ptr.Count := Item.Ptr.Count + 1;
      end if;
   end Adjust;

   function Allocate return Safe_Pointer is
      -- null;
   begin -- Allocate
      return Safe_Pointer'(Ada.Finalization.Controlled with Ptr => new Safe_Group);
   end Allocate;

   function Allocate (Data : Object) return Safe_Pointer is
      -- null;
   begin -- Allocate
      return Safe_Pointer'(Ada.Finalization.Controlled with Ptr => new Safe_Group'(Data => Data, Count => 1) );
   end Allocate;

   procedure Finalize (Item : in out Safe_Pointer) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Safe_Group, Name => Name);
   begin -- Finalize
      if Item.Ptr /= null then
         if Item.Ptr.Count > 0 then
            Item.Ptr.Count := Item.Ptr.Count - 1;
         end if;

         if Item.Ptr.Count = 0 then
            Free (Item.Ptr);
         end if;

         Item.Ptr := null;
      end if;
   end Finalize;

   function Get (Pointer : Safe_Pointer) return Object is
      -- null;
   begin -- Get
      return Pointer.Ptr.Data;
   end Get;

   procedure Put (Pointer : in Safe_Pointer; Value : in Object) is
      -- null;
   begin -- Put
      Pointer.Ptr.Data := Value;
   end Put;
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
