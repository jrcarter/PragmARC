-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- General purpose stack for general use
--
-- History:
-- 2025 Jul 01     J. Carter          V2.2--Use SPDX license format
-- 2020 Dec 01     J. Carter          V2.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.2--Make Length O(1)
-- 2016 Jun 01     J. Carter          V1.1--Eliminated unused type declarations
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

with PragmARC.Data_Structures.Stacks.Unbounded.Unprotected;

with System;

generic -- PragmARC.Data_Structures.Stacks.Unbounded.Protection
   type Element is private;
package PragmARC.Data_Structures.Stacks.Unbounded.Protection with Preelaborate is
   package Implementation is new Unprotected (Element => Element);

   protected type Handle (Ceiling_Priority : System.Any_Priority := System.Default_Priority) is -- Initial value: empty
      pragma Priority (Ceiling_Priority);

      procedure Clear with
         Post => Is_Empty;

      procedure Push (Item : in Element) with
         Post => not Is_Empty; -- raise Storage_Exhausted
      -- Adds Item to the top of the stack
      -- Raises Storage_Exhausted if there is insufficient storage for the Element
      -- The stack is unchanged if Storage_Exhausted is raised

      procedure Pop (Item : out Element); -- raise Empty
      -- Removes the top Element from the stack and puts it in Item
      -- Raises Empty if the stack has no elements
      -- The stack is unchanged if Empty is raised
      -- Contents of Item are undefined if Empty is raised
      --
      -- Precondition:  not Is_Empty     raises Empty if violated
      --
      -- Postcondition: after[Length] = before[Length] - 1

      function Is_Empty return Boolean;
      -- Returns True if the stack is empty; False otherwise

      function Length return Natural;
      -- Returns the number of Elements in the stack

      function Peek return Element;
      -- Returns the Element at the top of the stack without altering the stack
      -- Raises Empty if the stack is empty
      --
      -- Precondition:  not Is_Empty     raises Empty if violated

      procedure Iterate (Action : access procedure (Item : in Element) );
      -- Applies Action to each Element in the stack in turn, from top to bottom
   private -- Handle
      Stack : Implementation.Handle;
   end Handle;
end PragmARC.Data_Structures.Stacks.Unbounded.Protection;
