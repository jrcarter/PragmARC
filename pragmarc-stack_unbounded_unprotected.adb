-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 Jun 01     J. Carter          V1.2--Added Peek
-- 2001 May 01     J. Carter          V1.1--Improved time complexity of Is_Empty
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Stack_Unbounded_Unprotected is
   -- Conventions: The top    of a stack is the first Element in its List
   --              The bottom of a stack is the last  Element in its List

   use type Implementation.Position;

   procedure Clear (Stack : in out Handle) is
      -- null;
   begin -- Clear
      Implementation.Clear (List => Stack.List);
   end Clear;

   procedure Assign (To : out Handle; From : in Handle) is
      -- null;
   begin -- Assign
      Implementation.Assign (To => To.List, From => From.List);
   end Assign;

   function Is_Empty (Stack : Handle) return Boolean is
      -- null;
   begin -- Is_Empty
      return Implementation.Is_Empty (Stack.List);
   end Is_Empty;

   procedure Iterate (Over : in out Handle; Context : in out Context_Data) is
      procedure Action
         (Item : in out Element; Context : in out Context_Data; Pos : in Implementation.Position; Continue : out Boolean)
      is
         -- null;
      begin -- Action
         Action (Item => Item, Context => Context, Continue => Continue);
      end Action;

      procedure Local is new Implementation.Iterate (Context_Data => Context_Data, Action => Action);
   begin -- Iterate
      Local (Over => Over.List, Context => Context);
   end Iterate;

   function Length (Stack : Handle) return Natural is
      -- null;
   begin -- Length
      return Implementation.Length (Stack.List);
   end Length;

   function Peek (Stack : Handle) return Element is
      Pos : Implementation.Position := Implementation.First (Stack.List);
   begin -- Peek
      if Pos = Implementation.Off_List (Stack.List) then -- Precondition
         raise Empty;
      end if;

      return Implementation.Get (Stack.List, Pos);
   end Peek;

   procedure Pop (From : in out Handle; Item : out Element) is
      Pos : Implementation.Position := Implementation.First (From.List);
   begin -- Pop
      if Pos = Implementation.Off_List (From.List) then -- Precondition
         raise Empty;
      end if;

      Assign (To => Item, From => Implementation.Get (From.List, Pos) );
      Implementation.Delete (From => From.List, Pos => Pos);
   end Pop;

   procedure Push (Onto : in out Handle; Item : in Element) is
      Pos : Implementation.Position;
   begin -- Push
      Implementation.Insert (Into => Onto.List, Item => Item, Before => Implementation.First (Onto.List), New_Pos => Pos);
   end Push;
end PragmARC.Stack_Unbounded_Unprotected;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.