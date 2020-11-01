-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Bounded queue ADT for sequential use only
-- Each queue has a preset maximum size
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V2.1--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V2.0--Added Assign; implemented using PragmARC.List_Bounded_Unprotected
-- 2001 Jun 01     J. Carter          V1.1--Added Peek
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers.Bounded_Doubly_Linked_Lists;

generic -- PragmARC.Data_Structures.Queues.Bounded.Unprotected
   type Element is private;
package PragmARC.Data_Structures.Queues.Bounded.Unprotected is
   pragma Preelaborate;

   type Handle (Max_Size : Ada.Containers.Count_Type) is tagged limited private; -- Initial value: emtpy

   procedure Clear (Queue : in out Handle) with
      Post => Queue.Is_Empty;
   -- Makes Queue empty
   -- Contents of Queue are lost
   -- All queues are initially empty

   procedure Assign (To : out Handle; From : in Handle) with
      Pre => Integer (To.Max_Size) >= From.Length or else raise Too_Short;
   -- Makes To a copy of From

   procedure Put (Into : in out Handle; Item : in Element) with
      Pre  => not Into.Is_Full or else raise Full,
      Post => not Into.Is_Empty;
   -- Adds Item to Into

   procedure Get (From : in out Handle; Item : out Element) with
      Pre  => not From.Is_Empty or else raise Empty,
      Post => not From.Is_Full;
   -- Removes the next Element from From and puts it in Item

   function Is_Full (Queue : Handle) return Boolean;
   -- Returns True if Queue is full; False otherwise

   function Is_Empty (Queue : Handle) return Boolean;
   -- Returns True if Queue is empty; False otherwise

   function Length (Queue : Handle) return Natural;
   -- Returns the number of Elements in Queue

   function Peek (Queue : Handle) return Element with
      Pre => not Queue.Is_Empty or else raise Empty;
   -- Returnss the Element at the head of Queue without altering Queue

   generic -- Iterate
      with procedure Action (Item : in Element);
   procedure Iterate (Over : in out Handle);
   -- Applies Action to each Element in Over, from head to tail
private -- PragmARC.Data_Structures.Queues.Bounded.Unprotected
   package Implementation is new Ada.Containers.Bounded_Doubly_Linked_Lists (Element_Type => Element);

   type Handle (Max_Size : Ada.Containers.Count_Type) is tagged limited record
      List : Implementation.List (Capacity => Max_Size);
   end record;
end PragmARC.Data_Structures.Queues.Bounded.Unprotected;
