-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.2--Adhere to coding standard
-- 2021 Feb 01     J. Carter          V2.1--Removed Sqrt
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Aug 15     J. Carter          V1.1--Make use of improved accuracy in Rational_Numbers.Sqrt
-- 2017 Apr 15     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with PragmARC.Ansi_Tty_Control;
with PragmARC.Data_Structures.Stacks.Unbounded.Unprotected;
with PragmARC.Word_Input;

function PragmARC.Rational_Postfix_Calculator return PragmARC.Unbounded_Numbers.Rationals.Rational is
   use Ada;

   package Input is new Word_Input (Max_Word => 3_000);

   use Unbounded_Numbers.Rationals;

   subtype Real is Rational;

   package Real_Stack is new Data_Structures.Stacks.Unbounded.Unprotected (Element => Real);

   Display_Line : constant Positive :=  5;
   Input_Line   : constant Positive := 10;
   Error_Line   : constant Positive := 15;

   Stack        : Real_Stack.Handle;
   Left         : Real;
   Right        : Real;
   Result       : Real;
   Command      : Input.Word;

   package Ansi renames Ansi_Tty_Control;

   procedure Process_Result (Stack : in out Real_Stack.Handle; Result : in Real; As_Fraction : in Boolean := False);
   -- Push Result on Stack and display it

   procedure Get_Unary_Operand (Stack : in out Real_Stack.Handle; Left : out Real);
   -- Get the top of Stack in Left

   procedure Get_Binary_Operands (Stack : in out Real_Stack.Handle; Left : out Real; Right : out Real);
   -- Get the top two values from Stack into Left and Right

   procedure Process_Result (Stack : in out Real_Stack.Handle; Result : in Real; As_Fraction : in Boolean := False) is
    -- Empty
   begin -- Process_Result
      Stack.Push (Item => Result);

      Clear : for Line in Display_Line .. Input_Line - 1 loop
         Text_IO.Put (Item => Ansi.Position (Line, 1) & Ansi.Clear_End_Of_Line);
      end loop Clear;

      Text_IO.Put (Item => Ansi.Position (Display_Line, 1) & Ansi.Clear_End_Of_Line & Image (Result, As_Fraction => As_Fraction) );
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
   Text_IO.Put (Item => Ansi.Clear_Screen & "PragmAda Postfix Calculator");
   Process_Result (Stack => Stack, Result => Zero);
   Text_IO.Put (Item => Ansi.Position (16, 1) );
   Text_IO.Put_Line (Item => "Q:  Quit (exit calculator)");
   Text_IO.Put_Line (Item => "C:  Clear                                   FRAC:   display as fraction");
   Text_IO.Put_Line (Item => "S:  change Sign                             **:     exponentiation to small");
   Text_IO.Put_Line (Item => "+:  add                                             integer value");
   Text_IO.Put_Line (Item => "-:  subtract");
   Text_IO.Put_Line (Item => "*:  multiply");
   Text_IO.Put_Line (Item => "/:  divide");
   Text_IO.Put_Line (Item => "Numbers are put on the stack (nust begin with a digit)");
   Text_IO.Put      (Item => "Any other input is an error");

   All_Ops       : loop
      Handle_Errors : declare
      -- Empty
      begin -- Handle_Errors
         Text_IO.Put (Item => Ansi.Position (Input_Line, 1) & Ansi.Clear_End_Of_Line);
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
--              elsif Com_Str = "SQRT" then -- Square root
--                 Get_Unary_Operand (Stack => Stack, Left => Left);
--                 Text_IO.Put (Item => Ansi.Position (Input_Line, 1) & Ansi.Clear_End_Of_Line &
--                                      "Processing SQRT; this can take a while");
--                 Result := Sqrt (Left);
--                 Process_Result (Stack => Stack, Result => Result);
            elsif Com_Str = "FRAC" then -- Redisplay as fraction
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result, As_Fraction => True);
            elsif Com_Str'Length > 0 and then Com_Str (1) in '0' .. '9' then -- Number?
               Result := Value (Com_Str);
               Process_Result (Stack => Stack, Result => Result);
            else -- Error
               raise Unidentified;
            end if;

            Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line);
         exception -- Convert
         when Unidentified =>
            Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line & "Error:  Invalid input:  " & Com_Str);
            Get_Unary_Operand (Stack => Stack, Left => Result);
            Process_Result (Stack => Stack, Result => Result);
         when Storage_Exhausted =>
            Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line &
                                 "Error:  Stack is full (no room for this operand):  " & Com_Str);
            Get_Unary_Operand (Stack => Stack, Left => Result);
            Process_Result (Stack => Stack, Result => Result);
         when Empty =>
            Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line &
                                 "Error:  Stack is empty (not enough operands for operation):  " & Com_Str);
            Stack.Clear; -- Should not change anything
            Process_Result (Stack => Stack, Result => Zero);
         when Text_IO.Data_Error =>
            Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line & "Error:  Invalid number:  " & Com_Str);

            if Stack.Is_Empty then
               Stack.Clear;
               Process_Result (Stack => Stack, Result => Zero);
            else
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            end if;
         when Constraint_Error =>
            Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line & "Error:  Invalid result:  " & Com_Str);

            if Stack.Is_Empty then
               Stack.Clear;
               Process_Result (Stack => Stack, Result => Zero);
            else
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            end if;
         when E : others =>
               Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line &
                                    "Error:  Invalid operand:  " & Com_Str & ' ' & Ada.Exceptions.Exception_Information (E) );

            if Stack.Is_Empty then
               Stack.Clear;
               Process_Result (Stack => Stack, Result => Zero);
            else
               Get_Unary_Operand (Stack => Stack, Left => Result);
               Process_Result (Stack => Stack, Result => Result);
            end if;
         end Convert;
      exception -- Handle_Errors
      when Too_Short =>
         Text_IO.Put
               (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line & "Error:  Too many characters without whitespace");
         Get_Unary_Operand (Stack => Stack, Left => Result);
         Process_Result (Stack => Stack, Result => Result);
      when E : others => -- ?
         Text_IO.Put (Item => Ansi.Position (Error_Line, 1) & Ansi.Clear_End_Of_Line &
                              "Error:  Undefined error handled by Handle_Errors " & Ada.Exceptions.Exception_Information (E) );
         Get_Unary_Operand (Stack => Stack, Left => Result);
         Process_Result (Stack => Stack, Result => Result);
      end Handle_Errors;
   end loop All_Ops;
end PragmARC.Rational_Postfix_Calculator;
