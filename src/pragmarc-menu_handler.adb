-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part and formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Text_IO;
with PragmARC.Ansi_Tty_Control;

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

      package Choice_IO is new Text_IO.Integer_IO (Num => Choice_Id);

      procedure Error_Message is
         -- Empty
      begin -- Error_Message
         Text_IO.Put (Item => Ansi_Tty_Control.Position (Error_Line_Entry, Error_Column_Entry) &
                              Ansi_Tty_Control.Clear_End_Of_Line &
                              "*** Invalid choice - Please reenter ***");
      end Error_Message;
   begin -- Process
      -- Output centered and bold header
      Text_IO.Put (Item => Ansi_Tty_Control.Clear_Screen &
                           Ansi_Tty_Control.Position (Top_Line, 40 - V_String.Length (Menu.Header) / 2) &
                           Ansi_Tty_Control.Bold_Mode &
                           V_String.To_String (Menu.Header) &
                           Ansi_Tty_Control.Normal_Mode);

      -- Find length of longest line for centering purposes
      Longest_Line := V_String.Length (Menu.Item (1) );

      Long_Line : for I in 2 .. Menu.Num_Items loop
         if V_String.Length (Menu.Item (I) ) > Longest_Line then
            Longest_Line := V_String.Length (Menu.Item (I) );
         end if;
      end loop Long_Line;

      Choices_Column_Entry := Num_Columns / 2 - Longest_Line / 2;

      -- Output centered choices, with default in reverse video
      Display_Choices : for Counter in 1 .. Menu.Num_Items loop
         Text_IO.Put (Item => Ansi_Tty_Control.Position (Counter + 2, Choices_Column_Entry - Choice_Width_Default / 2) );
         Choice_IO.Put (Item => Counter, Width => Choice_Width_Default);
         Text_IO.Put (Item => "  ");

         if Menu.Default_Exists and then Counter = Menu.Default_Choice then
            Text_IO.Put (Item => Ansi_Tty_Control.Reverse_Video &
                                 V_String.To_String (Menu.Item (Counter) ) &
                                 Ansi_Tty_Control.Normal_Mode);
         else
            Text_IO.Put (Item => V_String.To_String (Menu.Item (Counter) ) );
         end if;
      end loop Display_Choices;

      Text_IO.Put (Item => Ansi_Tty_Control.Position (Choice_Line_Entry, Column_Entry) & "Enter Choice:");

      Valid_Selection : loop
         Text_IO.Put (Item => Ansi_Tty_Control.Position (Choice_Line_Entry, Choice_Column_Place) &
                              Ansi_Tty_Control.Clear_End_Of_Line);

         Get_Choice : declare
            Input  : constant String := Text_IO.Get_Line;

            Length : Natural;
         begin -- Get_Choice
            if Input'Length > 0 then
               Choice_IO.Get (From => Input, Item => Selection, Last => Length);

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
   end Process;
end PragmARC.Menu_Handler;
