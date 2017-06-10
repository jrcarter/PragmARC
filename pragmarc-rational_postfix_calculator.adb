-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2017 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2017 Apr 15     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with PragmARC.Ansi_Tty_Control;
with PragmARC.Stack_Unbounded;
with PragmARC.Word_Input;

use Ada;
function PragmARC.Rational_Postfix_Calculator return PragmARC.Rational_Numbers.Rational is
   package Input is new Word_Input (Max_Word => 3_000);

   use Rational_Numbers;

   subtype Real is Rational;

   package Real_Stack is new Stack_Unbounded (Element => Real);

   Accuracy_Factor : constant Real := Value ("0.0002");

   Display_Line : constant Positive :=  5;
   Input_Line   : constant Positive := 10;
   Error_Line   : constant Positive := 15;

   Stack        : Real_Stack.Handle;
   Left         : Real;
   Right        : Real;
   Result       : Real;
   Command      : Input.Word;

   package Ansi renames Ansi_Tty_Control;

   procedure Process_Result (Stack : in out Real_Stack.Handle; Result : in Real; As_Fraction : in Boolean := False) is
    -- Empty
   begin -- Process_Result
      Stack.Push (Item => Result);

      Clear : for Line in Display_Line .. Input_Line - 1 loop
         Text_Io.Put (Ansi.Position (Line, 1) & Ansi.Clear_End_Of_Line);
      end loop Clear;

      Text_Io.Put (Ansi.Position (Display_Line, 1) & Ansi.Clear_End_Of_Line & Image (Result, As_Fraction => As_Fraction) );
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
begin -- PragmARC.Rational_Postfix_Calculator
   Text_Io.Put (Ansi.Clear_Screen & "PragmAda Postfix Calculator");
   Process_Result (Stack => Stack, Result => Zero);
   Text_Io.Put (Ansi.Position (16, 1) );
   Text_Io.Put_Line ("Q:  Quit (exit calculator)                  SQRT:   square root");
   Text_Io.Put_Line ("C:  Clear                                   FRAC:   display as fraction");
   Text_Io.Put_Line ("S:  change Sign                             **:     exponentiation to small integer value");
   Text_Io.Put_Line ("+:  add");
   Text_Io.Put_Line ("-:  subtract");
   Text_Io.Put_Line ("*:  multiply");
   Text_Io.Put_Line ("/:  divide");
   Text_Io.Put_Line ("Numbers are put on the stack");
   Text_Io.Put      ("Any other input is an error");

   All_Ops       : loop
      Handle_Errors : declare
      -- Empty
      begin -- Handle_Errors
         Text_Io.Put (Ansi.Position (Input_Line, 1) & Ansi.Clear_End_Of_Line);
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
               Process_Result (Stack => Stack, Result => Zero);
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

               Power : declare
                  Img : constant String  := Image (Right);
                  Dot : constant Natural := Ada.Strings.Fixed.Index (Img, ".");
                  Pow : constant Integer := Integer'Value (Img (Img'First .. Dot - 1) );
               begin -- Power
                  Result := Left ** Pow;
                  Process_Result (Stack => Stack, Result => Result);
               end Power;
            elsif Com_Str = "SQRT" then -- Square root
               Get_Unary_Operand (Stack => Stack, Left => Left);
               Text_Io.Put (Item => Ansi.Position (Input_Line, 1) & Ansi.Clear_End_Of_Line & "Processing SQRT; this can take a while");
               Result := Sqrt (Left, Left * Accuracy_Factor);
               Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "EXP" then -- Power of 'e'
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Exp (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "LOG" then -- Natural log
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Log (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "SIN" then -- Sine
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Sin (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "COS" then -- Cosine
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Cos (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "TAN" then -- Tangent
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Tan (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "ARCSIN" then -- Rrcsine
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Arcsin (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "ARCCOS" then -- Arccosine
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Arccos (Left);
--            Process_Result (Stack => Stack, Result => Result);
--          elsif Com_Str = "ARCTAN" then -- Arctangent
--            Get_Unary_Operand (Stack => Stack, Left => Left);
--            Result := Arctan (Left);
--            Process_Result (Stack => Stack, Result => Result);
            elsif Com_Str = "FRAC" then -- Redisplay as fraction
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result, As_Fraction => True);
            elsif Com_Str'Length > 0 and then Com_Str (1) in '0' .. '9' then -- Number?
               Result := Value (Com_Str);
               Process_Result (Stack => Stack, Result => Result);
            else -- Error
               raise Unidentified;
            end if;

            Text_Io.Put (Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line);
         exception -- Convert
         when Unidentified =>
            Text_Io.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid input:  " &
                            Com_Str);
            Get_Unary_Operand (Stack => Stack, Left => Result);
            Process_Result (Stack => Stack, Result => Result);
         when Storage_Exhausted =>
            Text_Io.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Stack is full (no room for this operand):  " &
                            Com_Str);
            Get_Unary_Operand (Stack => Stack, Left => Result);
            Process_Result (Stack => Stack, Result => Result);
         when Empty =>
            Text_Io.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Stack is empty (not enough operands for operation):  " &
                            Com_Str);
            Stack.Clear; -- Should not change anything
            Process_Result (Stack => Stack, Result => Zero);
         when Text_Io.Data_Error =>
            Text_Io.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid number:  " &
                            Com_Str);

            if Stack.Is_Empty then
               Stack.Clear;
               Process_Result (Stack => Stack, Result => Zero);
            else
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            end if;
         when Constraint_Error =>
            Text_Io.Put (Ansi.Position (Error_Line, 1) &
                            Ansi.Clear_End_Of_Line &
                            "Error:  Invalid result:  " &
                            Com_Str);

            if Stack.Is_Empty then
               Stack.Clear;
               Process_Result (Stack => Stack, Result => Zero);
            else
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            end if;
         when E : others =>
            Text_Io.Put (Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line & "Error:  Invalid operand:  " & Com_Str & ' ' &
                         Ada.Exceptions.Exception_Information (E) );

            if Stack.Is_Empty then
               Stack.Clear;
               Process_Result (Stack => Stack, Result => Zero);
            else
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            end if;
         end Convert;
      exception -- Handle_Errors
      when Input.Word_Too_Long =>
         Text_Io.Put (Ansi.Position (Error_Line, 1) &
                         Ansi.Clear_End_Of_Line &
                         "Error:  Too many characters without whitespace");
         Get_Unary_Operand (Stack => Stack, Left => Result);
         Process_Result (Stack => Stack, Result => Result);
      when E : others => -- ?
         Text_Io.Put (Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line &
                      "Error:  Undefined error handled by Handle_Errors " & Ada.Exceptions.Exception_Information (E) );
         Get_Unary_Operand (Stack => Stack, Left => Result);
         Process_Result (Stack => Stack, Result => Result);
      end Handle_Errors;
   end loop All_Ops;
end PragmARC.Rational_Postfix_Calculator;
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
