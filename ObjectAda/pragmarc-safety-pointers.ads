-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides "safe" pointers; pointers that deallocate themselves automatically
-- when they go out of scope and the designated object has no more references.
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

private with Ada.Finalization;

generic -- PragmARC.Safety.Pointers
   type Object is private;
package PragmARC.Safety.Pointers is
   type Safe_Pointer is tagged private;

   Null_Pointer : constant Safe_Pointer;

   function Allocate return Safe_Pointer;
   -- Equivalent to "new Object".
   -- May raise Storage_Error.

   function Allocate (Data : in Object) return Safe_Pointer;
   -- Equivalent to "new Object'(Data)".
   -- May raise Storage_Error.

   function Get (Pointer : in Safe_Pointer) return Object with
      Pre => Pointer /= Null_Pointer or else raise Constraint_Error;
   -- Equivalent to "Pointer.all".

   procedure Put (Pointer : in Safe_Pointer; Value : in Object) with
      Pre  => Pointer /= Null_Pointer or else raise Constraint_Error,
      Post => Get (Pointer) = Value;
   -- Equivalent to "Pointer.all := Value;".

   function "=" (Left : in Safe_Pointer; Right : in Safe_Pointer) return Boolean;
   -- Returns True if Left and Right have the same access value; False otherwise.
private -- PragmARC.Safety.Pointers
   type Safe_Group;

   type Name is access Safe_Group;

   type Safe_Pointer is new Ada.Finalization.Controlled with record
      Ptr : Name;
   end record;

   procedure Adjust   (Item : in out Safe_Pointer);
   procedure Finalize (Item : in out Safe_Pointer);

   Null_Pointer : constant Safe_Pointer := Safe_Pointer'(Ada.Finalization.Controlled with Ptr => null);
end PragmARC.Safety.Pointers;
