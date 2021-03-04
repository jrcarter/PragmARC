-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2001 Jun 01     J. Carter          V1.3--Added Peek
-- 2001 May 01     J. Carter          V1.2--Improved time complexity of Is_Empty
-- 2000 Dec 01     J. Carter          V1.1--Revised implementation of Iterate
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Stacks.Unbounded.Protection is
   protected body Handle is
      procedure Clear is
         -- Empty
      begin -- Clear
         Stack.Clear;
      end Clear;

      procedure Push (Item : in Element) is
         -- Empty
      begin -- Push
         Stack.Push (Item => Item);
      end Push;

      procedure Pop (Item : out Element) is
         -- Empty
      begin -- Pop
         Stack.Pop (Item => Item);
      end Pop;

      function Is_Empty return Boolean is
         (Stack.Is_Empty);

      function Length return Natural is
         (Stack.Length);

      function Peek return Element is
         (Stack.Peek);

      procedure Iterate (Action : access procedure (Item : in Element) ) is
         procedure Local is new Implementation.Iterate (Action => Action.all);
      begin -- Iterate
         Local (Over => Stack);
      end Iterate;
   end Handle;
end PragmARC.Data_Structures.Stacks.Unbounded.Protection;
