-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2022 Apr 01     J. Carter          V2.2--Allow pointers to indefinite types
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.3--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.2--Added exception handler to Finalize
-- 2011 Jul 01     J. Carter          V1.1--Finalize may be called multiple times
-- 2005 Jul 01     J. Carter          V1.0--Initial release
--
with Ada.Unchecked_Deallocation;

package body PragmARC.Safety.Pointers is
   type Object_Ptr is access Object;

   type Safe_Group is record
      Data  : Object_Ptr;
      Count : Natural := 1;
   end record;

   procedure Adjust (Item : in out Safe_Pointer) is
      -- Empty
   begin -- Adjust
      if Item.Ptr /= null then
         Item.Ptr.Count := Item.Ptr.Count + 1;
      end if;
   end Adjust;

   function Allocate (Data : Object) return Safe_Pointer is
      (Ada.Finalization.Controlled with Ptr => new Safe_Group'(Data => new Object'(Data), Count => 1) );

   procedure Finalize (Item : in out Safe_Pointer) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Object, Name => Object_Ptr);
      procedure Free is new Ada.Unchecked_Deallocation (Object => Safe_Group, Name => Name);
   begin -- Finalize
      if Item.Ptr /= null then
         if Item.Ptr.Count > 0 then
            Item.Ptr.Count := Item.Ptr.Count - 1;
         end if;

         if Item.Ptr.Count = 0 then
            Free (Item.Ptr.Data);
            Free (Item.Ptr);
         end if;

         Item.Ptr := null;
      end if;
   exception -- Finalize
   when others =>
      null;
   end Finalize;

   function Get (Pointer : in Safe_Pointer) return Object is
      (Pointer.Ptr.Data.all);

   procedure Put (Pointer : in Safe_Pointer; Value : in Object) is
      -- Empty
   begin -- Put
      Pointer.Ptr.Data.all := Value;
   end Put;
end PragmARC.Safety.Pointers;
