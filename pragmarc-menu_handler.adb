-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Ansi_Tty_Control;
with PragmARC.Get_Line;
with Ada.Text_Io;

use Ada;
package body PragmARC.Menu_Handler is
   function Process (Menu : Menu_Info) return Choice_Id is
      Choice_Column_Place  : constant := 20; -- Column for user's choice entry
      Choice_Line_Entry    : constant := 23; -- Line for user's choice entry
      Choice_Width_Default : constant :=  2; -- Width of displayed numbers
      Column_Entry         : constant :=  5; -- Column for user prompt
      Error_Column_Entry   : constant :=  3; -- Error descriptions begin here
      Error_Line_Entry     : constant := 22; -- Line for error descriptions
      Top_Line             : constant :=  1; -- Top line of screen

      Choices_Column_Entry : Positive; -- Column where choices displayed
      Selection            : Choice_Id;
      Longest_Line         : Natural;

      package Choice_Io is new Text_Io.Integer_Io (Choice_Id);

      procedure Error_Message is
         -- null;
      begin -- Error_Message
         Text_Io.Put (Ansi_Tty_Control.Position (Error_Line_Entry, Error_Column_Entry) &
                      Ansi_Tty_Control.Clear_End_Of_Line &
                      "*** Invalid choice - Please reenter ***"
                     )
         ;
      end Error_Message;
   begin -- Process
      -- Output centered and bold header
      Text_Io.Put (Ansi_Tty_Control.Clear_Screen &
                   Ansi_Tty_Control.Position (Top_Line, 40 - V_String.Length (Menu.Header) / 2) &
                   Ansi_Tty_Control.Bold_Mode &
                   V_String.To_String (Menu.Header) &
                   Ansi_Tty_Control.Normal_Mode
                  )
      ;

      -- Find length of longest line for centering purposes
      Longest_Line := V_String.Length (Menu.Item (1) );

      Long_Line : for I in 2 .. Menu.Num_Items loop
         if V_String.Length (Menu.Item (I) ) > Longest_Line then
            Longest_Line := V_String.Length (Menu.Item (I) );
         end if;
      end loop Long_Line;

      Choices_Column_Entry := 40 - Longest_Line / 2;

      -- Output centered choices, with default in reverse video
      Display_Choices : for Counter in 1 .. Menu.Num_Items loop
         Text_Io.Put (Ansi_Tty_Control.Position (Counter + 2, Choices_Column_Entry - Choice_Width_Default / 2) );
         Choice_Io.Put (Item => Counter, Width => Choice_Width_Default);
         Text_Io.Put ("  ");

         if Menu.Default_Exists and then Counter = Menu.Default_Choice then
            Text_Io.Put (Ansi_Tty_Control.Reverse_Video &
                         V_String.To_String (Menu.Item (Counter) ) &
                         Ansi_Tty_Control.Normal_Mode
                        )
            ;
         else
            Text_Io.Put (V_String.To_String (Menu.Item (Counter) ) );
         end if;
      end loop Display_Choices;

      Text_Io.Put (Ansi_Tty_Control.Position (Choice_Line_Entry, Column_Entry) & "Enter Choice:");

      Valid_Selection : loop
         Text_Io.Put (Ansi_Tty_Control.Position (Choice_Line_Entry, Choice_Column_Place) &
                      Ansi_Tty_Control.Clear_End_Of_Line
                     )
         ;

         Get_Choice : declare
            Input  : constant String := Get_Line;

            Length : Natural;
         begin -- Get_Choice
            if Input'Length > 0 then
               Choice_Io.Get (From => Input, Item => Selection, Last => Length);

               if Selection in 1 .. Menu.Num_Items then
                  return Selection;
               end if;

               Error_Message;
            elsif Menu.Default_Exists then
               return Menu.Default_Choice;
            else
               null;
            end if;
         exception -- Get_Choice
         when others =>
            Error_Message;
         end Get_Choice;
      end loop Valid_Selection;

      return Selection;
   end Process;
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