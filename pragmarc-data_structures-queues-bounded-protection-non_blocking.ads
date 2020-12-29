-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Bounded queue ADT for general use
-- Each queue has a preset maximum size
--
-- History:
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate; use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.2--Added Ceiling_Priority to Handle
-- 2001 Jun 01     J. Carter          V1.1--Added Peek
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Containers;

with PragmARC.Data_Structures.Queues.Bounded.Unprotected;

with System;

generic -- PragmARC.Data_Structures.Queues.Bounded.Protection.Non_Blocking
   type Element is private;
package PragmARC.Data_Structures.Queues.Bounded.Protection.Non_Blocking with Preelaborate is
   package Implementation is new Unprotected (Element => Element);

   protected type Handle (Max_Size : Ada.Containers.Count_Type; Ceiling_Priority : System.Any_Priority) is -- Initial value: emtpy
      pragma Priority (Ceiling_Priority);

      procedure Clear with
         Post => Is_Empty;
      -- Contents of the queue are lost
      -- The queue is initially empty

      procedure Put (Item : in Element) with
         Post => not Is_Empty; -- raise Full
      -- Adds Item to the queue
      -- Raises Full the queue is already full
      -- The queue is unchanged if Full is raised
      --
      -- Precondition:  not Is_Full     raise Full if violated

      procedure Get (Item : out Element) with
         Post => not Is_Full; -- raise Empty
      -- Removes the next Element from the queue and puts it in Item
      -- Raises Empty if the queue is empty
      -- The queue is unchanged if Empty is raised
      -- Contents of Item are undefined if Empty is raised
      --
      -- Precondition:  not Is_Empty     raise Empty if violated

      function Is_Full return Boolean;
      -- Returns True if the queue is full; False otherwise

      function Is_Empty return Boolean;
      -- Returns True if the queue is empty; False otherwise

      function Length return Natural; -- Returns the number of Elements in the queue

      function Peek return Element;
      -- Returns the Element at the head of the queue without altering the queue
      -- Raises Empty if the queue is empty
      --
      -- Precondition:  not Is_Empty     raise Empty if violated

      procedure Iterate (Action : access procedure (Item : in Element) );
      -- Applies Action to each Element in the queue in turn, from head to tail.
   private -- Handle
      Queue : Implementation.Handle (Max_Size => Max_Size);
   end Handle;
end PragmARC.Data_Structures.Queues.Bounded.Protection.Non_Blocking;
