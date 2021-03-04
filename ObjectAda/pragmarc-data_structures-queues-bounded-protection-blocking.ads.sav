-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Bounded queue ADT for concurrent use only
-- Each queue has a preset maximum size
-- A call to Put when the queue is full  blocks the caller until another task calls Get
-- A call to Get when the queue is empty blocks the caller until another task calls Put
-- Implements a single-instantiation form of Ada.Containers.Bounded_Synchronized_Queues
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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

generic -- PragmARC.Data_Structures.Queues.Bounded.Protection.Blocking
   type Element is private;
package PragmARC.Data_Structures.Queues.Bounded.Protection.Blocking with Preelaborate is
   package Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Element);

   package Queues is new Ada.Containers.Bounded_Synchronized_Queues (Queue_Interfaces => Interfaces, Default_Capacity => 1000);
   -- Type Queues.Queue implements the queues
end PragmARC.Data_Structures.Queues.Bounded.Protection.Blocking;

