-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- General purpose queue for general use
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.1--Make Length O(1)
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.3--Added Ceiling_Priority to Handle
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Data_Structures.Queues.Unbounded.Unprotected;

with System;

generic -- PragmARC.Data_Structures.Queues.Unbounded.Protection.Non_Blocking
   type Element is private;
package PragmARC.Data_Structures.Queues.Unbounded.Protection.Non_Blocking is
   pragma Preelaborate;

   package Implementation is new Unprotected (Element => Element);

   protected type Handle (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is -- Initial value: empty
      pragma Priority (Ceiling_Priority);

      procedure Clear with
         Post => Is_Empty;

      procedure Put (Item : in Element) with
         Post => not Is_Empty; -- raise Storage_Exhausted
      -- Adds Item to the queue
      -- Raises Storage_Exhausted if there is insufficient storage for the Element
      -- The queue is unchanged if Storage_Exhausted is raised

      procedure Get (Item : out Element); -- raise Empty
      -- Removes the next Element from the queue and puts it in Item
      -- Raises Empty if the queue has no elements
      -- The queue is unchanged if Empty is raised
      -- Contents of Item are undefined if Empty is raised
      --
      -- Precondition:  not Is_Empty     raise Empty if violated
      --
      -- Postcondition: after[Length] = before[Length] - 1

      function Is_Empty return Boolean;
      -- Returns True if the queue is empty; False otherwise

      function Length return Natural;
      -- Returns the number of Elements in the queue

      function Peek return Element;
      -- Returns the Element at the head of the queue without altering the queue
      -- Raises Empty if the queue is empty
      --
      -- Precondition:  not Is_Empty     raise Empty if violated

      procedure Iterate (Action : access procedure (Item : in Element) );
      -- Applies Action to each Element in the queue in turn, from head to tail
   private -- Handle
      Queue : Implementation.Handle;
   end Handle;
end PragmARC.Data_Structures.Queues.Unbounded.Protection.Non_Blocking;
