-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part and formatting
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.1--Use Real for Element of stack
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with PragmARC.Ansi_Tty_Control;
with PragmARC.Data_Structures.Stacks.Unbounded.Unprotected;
with PragmARC.Word_Input;

package body PragmARC.Postfix_Calculator is
   use Ada;

   package Input is new Word_Input (Max_Word => System.Max_Digits + System.Max_Digits / 2);

   package Real_Stack is new Data_Structures.Stacks.Unbounded.Unprotected (Element => Real);

   package Real_Math is new Ada.Numerics.Generic_Elementary_Functions (Float_Type => Real);

   function Calculator return Real is
      Display_Line : constant Positive :=  5;
      Input_Line   : constant Positive :=  8;
      Error_Line   : constant Positive := 11;

      Stack        : Real_Stack.Handle;
      Left         : Real;
      Right        : Real;
      Result       : Real;
      Command      : Input.Word;

      package Ansi renames Ansi_Tty_Control;

      procedure Process_Result (Stack : in out Real_Stack.Handle; Result : in Real);
      -- Puts Result on Stack and displays it

      procedure Get_Unary_Operand (Stack : in out Real_Stack.Handle; Left : out Real);
      -- Gets the top of Stack into Left

      procedure Get_Binary_Operands (Stack : in out Real_Stack.Handle; Left : out Real; Right : out Real);
      -- Gets the top two Reals from Stack into Left and Right

      procedure Process_Result (Stack : in out Real_Stack.Handle; Result : in Real) is
         -- Empty
      begin -- Process_Result
         Stack.Push (Item => Result);
         Text_IO.Put (Ansi.Position (Display_Line, 1) & Ansi.Clear_End_Of_Line & Real'Image (Result) );
      end Process_Result;

      procedure Get_Unary_Operand (Stack : in out Real_Stack.Handle; Left : out Real) is
         -- Empty
      begin -- Get_Unary_Operand
         Stack.Pop (Item => Left);
      end Get_Unary_Operand;

      procedure Get_Binary_Operands (Stack : in out Real_Stack.Handle; Left : out Real; Right : out Real) is
         -- Empty
      begin -- Get_Binary_Operands
         Stack.Pop (Item => Right);
         Stack.Pop (Item => Left);
      end Get_Binary_Operands;

      use Real_Math;
   begin -- Calculator
      Text_IO.Put (Ansi.Clear_Screen & "PragmAda Postfix Calculator");
      Process_Result (Stack => Stack, Result => 0.0);
      Text_IO.Put (Ansi.Position (15, 1) );
      Text_IO.Put_Line ("Q:  Quit (exit calculator)                  SQRT:   square root");
      Text_IO.Put_Line ("C:  Clear                                   EXP:    power of 'e'");
      Text_IO.Put_Line ("S:  change Sign                             LOG:    natural log");
      Text_IO.Put_Line ("+:  add                                     SIN:    sine");
      Text_IO.Put_Line ("-:  subtract                                COS:    cosine");
      Text_IO.Put_Line ("*:  multiply                                TAN:    tangent");
      Text_IO.Put_Line ("/:  divide                                  ARCSIN: arcsine");
      Text_IO.Put_Line ("**: power (positive value ** any value)     ARCCOS: arccosine");
      Text_IO.Put_Line ("Numbers are put on the stack                ARCTAN: arctangent");
      Text_IO.Put_Line ("Any other input is an error");

      All_Ops : loop
         Handle_Errors : declare
            -- Empty
         begin -- Handle_Errors
            Text_IO.Put (Ansi.Position (Input_Line, 1) & Ansi.Clear_End_Of_Line);
            Input.Get (Value => Command);

            Convert : declare
               Com_Str : constant String := Characters.Handling.To_Upper (Input.V_String.To_String (Command) );

               Unidentified : exception; -- Allows uniform processing of errors
            begin -- Convert
               if Com_Str = "Q" then -- Quit
                  Get_Unary_Operand (Stack => Stack, Left => Left);

                  return Left;
               elsif Com_Str = "C" then -- Clear
                  Stack.Clear;
                  Process_Result (Stack => Stack, Result => 0.0);
               elsif Com_Str = "S" then -- Change sign
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := -Left;
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "+" then -- Add
                  Get_Binary_Operands (Stack => Stack, Left => Left, Right => Right);
                  Result := Left + Right;
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "-" then -- Subtract
                  Get_Binary_Operands (Stack => Stack, Left => Left, Right => Right);
                  Result := Left - Right;
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "*" then -- Multiply
                  Get_Binary_Operands (Stack => Stack, Left => Left, Right => Right);
                  Result := Left * Right;
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "/" then -- Divide
                  Get_Binary_Operands (Stack => Stack, Left => Left, Right => Right);
                  Result := Left / Right;
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "**" then -- Power
                  Get_Binary_Operands (Stack => Stack, Left => Left, Right => Right);
                  Result := Left ** Right;
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "SQRT" then -- Square root
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Sqrt (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "EXP" then -- Power of 'e'
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Exp (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "LOG" then -- Natural log
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Log (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "SIN" then -- Sine
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Sin (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "COS" then -- Cosine
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Cos (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "TAN" then -- Tangent
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Tan (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "ARCSIN" then -- Rrcsine
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Arcsin (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "ARCCOS" then -- Arccosine
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Arccos (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str = "ARCTAN" then -- Arctangent
                  Get_Unary_Operand (Stack => Stack, Left => Left);
                  Result := Arctan (Left);
                  Process_Result (Stack => Stack, Result => Result);
               elsif Com_Str'Length > 0 and then Com_Str (1) in '0' .. '9' then -- Number?
                  Result := Real'Value (Com_Str);
                  Process_Result (Stack => Stack, Result => Result);
               else -- Error
                  raise Unidentified;
               end if;

               Text_IO.Put (Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line);
            exception -- Convert
            when Unidentified =>
               Text_IO.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid input:  " &
                            Com_Str);
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            when Storage_Exhausted =>
               Text_IO.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Stack is full (no room for this operand):  " &
                            Com_Str);
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            when Empty =>
               Text_IO.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Stack is empty (not enough operands for operation):  " &
                            Com_Str);
               Stack.Clear; -- Should not change anything
               Process_Result (Stack => Stack, Result => 0.0);
            when Text_IO.Data_Error =>
               Text_IO.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid number:  " &
                            Com_Str);

               if Stack.Is_Empty then
                  Stack.Clear;
                  Process_Result (Stack => Stack, Result => 0.0);
               else
                  Get_Unary_Operand (Stack => Stack, Left => Result);
                  Process_Result (Stack => Stack, Result => Result);
               end if;
            when Constraint_Error =>
               Text_IO.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid result:  " &
                            Com_Str);

               if Stack.Is_Empty then
                  Stack.Clear;
                  Process_Result (Stack => Stack, Result => 0.0);
               else
                  Get_Unary_Operand (Stack => Stack, Left => Result);
                  Process_Result (Stack => Stack, Result => Result);
               end if;
            when others =>
               Text_IO.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid operand:  " &
                            Com_Str);

               if Stack.Is_Empty then
                  Stack.Clear;
                  Process_Result (Stack => Stack, Result => 0.0);
               else
                  Get_Unary_Operand (Stack => Stack, Left => Result);
                  Process_Result (Stack => Stack, Result => Result);
               end if;
            end Convert;
         exception -- Handle_Errors
         when Too_Short =>
            Text_IO.Put (Ansi.Position (Error_Line, 1) &
                         Ansi.Clear_End_Of_Line &
                         "Error:  Too many characters without whitespace");
            Get_Unary_Operand (Stack => Stack, Left => Result);
            Process_Result (Stack => Stack, Result => Result);
         when others => -- ?
            Text_IO.Put (Ansi.Position (Error_Line, 1) &
                         Ansi.Clear_End_Of_Line &
                         "Error:  Undefined error handled by Handle_Errors");
            Get_Unary_Operand (Stack => Stack, Left => Result);
            Process_Result (Stack => Stack, Result => Result);
         end Handle_Errors;
      end loop All_Ops;
   end Calculator;
end PragmARC.Postfix_Calculator;
