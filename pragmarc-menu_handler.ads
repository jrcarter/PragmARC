-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Straightforward text menus for ANSI-standard terminals
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Bounded;

use Ada;
generic -- PragmARC.Menu_Handler
   Num_Columns : Positive := 80; -- Size of screen
   Num_Lines   : Positive := 25; -- Defaults for standard 25-line by 80 column screen
package PragmARC.Menu_Handler is
   Max_Choices : constant Positive := Num_Lines - 5;   -- Maximum # of choices which can be displayed to user
   Max_Length  : constant Positive := Num_Columns - 5; -- Maximum length of each choice

   package V_String is new Strings.Bounded.Generic_Bounded_Length (Max => Max_Length);
   subtype Choice_String is V_String.Bounded_String;

   subtype Choice_Id is Positive range 1 .. Max_Choices;
   type Choice_Set is array (Choice_Id range <>) of V_String.Bounded_String;

   type Menu_Info (Num_Items : Choice_Id; Default_Exists : Boolean) is record
      Header : V_String.Bounded_String;
      Item : Choice_Set (1 .. Num_Items);

      case Default_Exists is
      when False =>
         null;
      when True =>
         Default_Choice : Choice_Id := 1;
      end case;
   end record; -- If Default_Exists then entry of a null string by user is the same as selecting default choice

   function Process (Menu : Menu_Info) return Choice_Id; -- Displays Menu and obtains valid user selection
end PragmARC.Menu_Handler;
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