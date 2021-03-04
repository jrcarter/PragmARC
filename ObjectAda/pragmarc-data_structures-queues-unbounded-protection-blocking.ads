-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Blocking queue for concurrent use
-- Attempts to Get from an empty queue block until another task adds an Element to the queue
-- Implements a single-instantiation form of Ada.Containers.Unbounded_Synchronized_Queues
--
-- History:
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

generic -- PragmARC.Data_Structures.Queues.Unbounded.Protection.Blocking
   type Element is private;
package PragmARC.Data_Structures.Queues.Unbounded.Protection.Blocking with Preelaborate is
   package Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Element);

   package Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Interfaces);
   -- Type Queues.Queue implements the queues
end PragmARC.Data_Structures.Queues.Unbounded.Protection.Blocking;
