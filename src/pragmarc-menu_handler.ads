-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Straightforward text menus for ANSI-standard terminals
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Strings.Bounded;

generic -- PragmARC.Menu_Handler
   Num_Columns : Positive := 80; -- Size of screen
   Num_Lines   : Positive := 25; -- Defaults for standard 25-line by 80 column screen
package PragmARC.Menu_Handler is
   use Ada;

   Max_Choices : constant Positive := Num_Lines - 5;   -- Maximum # of choices which can be displayed to user
   Max_Length  : constant Positive := Num_Columns - 5; -- Maximum length of each choice

   package V_String is new Strings.Bounded.Generic_Bounded_Length (Max => Max_Length);
   subtype Choice_String is V_String.Bounded_String;

   subtype Choice_Id is Positive range 1 .. Max_Choices;
   type Choice_Set is array (Choice_Id range <>) of V_String.Bounded_String;

   type Menu_Info (Num_Items : Choice_Id; Default_Exists : Boolean) is record
      Header : V_String.Bounded_String;
      Item   : Choice_Set (1 .. Num_Items);

      case Default_Exists is
      when False =>
         null;
      when True =>
         Default_Choice : Choice_Id := 1;
      end case;
   end record; -- If Default_Exists then entry of a null string by user is the same as selecting default choice

   function Process (Menu : Menu_Info) return Choice_Id with
      Post => Process'Result in 1 .. Menu.Num_Items; -- Displays Menu and obtains valid user selection
end PragmARC.Menu_Handler;
