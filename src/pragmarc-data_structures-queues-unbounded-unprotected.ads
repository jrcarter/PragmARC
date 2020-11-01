-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- General purpose queue for sequential use
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Doubly_Linked_Lists;

generic -- PragmARC.Data_Structures.Queues.Unbounded.Unprotected
   type Element is private;
package PragmARC.Data_Structures.Queues.Unbounded.Unprotected is
   pragma Preelaborate;

   type Handle is tagged limited private; -- Initial value: empty

   procedure Clear (Queue : in out Handle) with
      Post => Queue.Is_Empty;
   -- Makes Queue empty

   procedure Assign (To : out Handle; From : in Handle);
   -- Makes To a copy of From
   -- May raise Storage_Exhausted
   -- The state of To is unknown if Storage_Exhausted is raised

   procedure Put (Onto : in out Handle; Item : in Element) with
      Post => not Onto.Is_Empty; -- raise Storage_Exhausted
   -- Adds Item to the tail of Onto
   -- Raises Storage_Exhausted if no more storage is available for Onto
   -- Nothing is changed if Storage_Exhausted is raised

   procedure Get (From : in out Handle; Item : out Element) with
      Pre  => not From.Is_Empty or else raise Empty;
   -- Post => From.Length = From'Old.Length + 1
   -- Removes the Element at the head of From and assigns it to Item

   function Length (Queue : Handle) return Natural;
   -- Returns the number of Elements in Queue

   function Is_Empty (Queue : Handle) return Boolean;
   -- Returns True if Queue is empty; False otherwise

   function Peek (Queue : Handle) return Element with
      Pre => not Queue.Is_Empty or else raise Empty;
   -- Return the Element at the head of Queue without altering Queue

   generic -- Iterate
      with procedure Action (Item : in Element);
   procedure Iterate (Over : in out Handle);
   -- Calls Action with each Element in Over in turn, from head to tail
private -- PragmARC.Data_Structures.Queues.Unbounded.Unprotected
   package Implementation is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Element);

   type Handle is tagged limited record
      List : Implementation.List;
   end record;
end PragmARC.Data_Structures.Queues.Unbounded.Unprotected;
