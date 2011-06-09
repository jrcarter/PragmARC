-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2001 Jun 01     J. Carter          V1.3--Added Peek
-- 2001 May 01     J. Carter          V1.2--Improved time complexity of Is_Empty
-- 2000 Dec 01     J. Carter          V1.1--Revised implementation of Iterate
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Stack_Unbounded is
   protected body Handle is
      procedure Clear is
         -- null;
      begin -- Clear
         Implementation.Clear (Stack => Stack);
      end Clear;

      procedure Push (Item : in Element) is
         -- null;
      begin -- Push
         Implementation.Push (Item => Item, Onto => Stack);
      end Push;

      procedure Pop (Item : out Element) is
         -- null;
      begin -- Pop
         Implementation.Pop (Item => Item, From => Stack);
      end Pop;

      function Is_Empty return Boolean is
         -- null;
      begin -- Is_Empty
         return Implementation.Is_Empty (Stack);
      end Is_Empty;

      function Length return Natural is
         -- null;
      begin -- Length
         return Implementation.Length (Stack);
      end Length;

      function Peek return Element is
         -- null;
      begin -- Peek
         return Implementation.Peek (Stack);
      end Peek;

      procedure Iterate (Action : in Action_Ptr; Context : in out Context_Data'Class) is
         procedure Local is new Implementation.Iterate (Context_Data => Context_Data'Class, Action => Action.all);
      begin -- Iterate
         Local (Over => Stack, Context => Context);
      end Iterate;
   end Handle;
end PragmARC.Stack_Unbounded;
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