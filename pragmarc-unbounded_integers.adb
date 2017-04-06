-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2017 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2017 Apr 15     J. Carter          V1.1--Added GCD and LCM
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body PragmARC.Unbounded_Integers is
   Radix : constant := Digit_Value'Modulus;

   use type Digit_List;

   function El_Or_0 (List : Digit_List; Index : Positive) return Digit_Value;
   -- If Index is a valid index into List, returns the value stored there; otherwise, returns 0

   procedure Insert (List : in out Digit_List; Index : in Positive; Value : in Digit_Value);
   -- If Index is a valid index into List, replaces the digit at Index with Value;
   -- otherwise, appends zeros to List until Index - 1 is a valid index, then appends Value to List

   procedure Reduce_Digits (Value : in out Unbounded_Integer);
   -- Deletes unnecessary zero digits from Value.Digit

   procedure Divide (Left      : in     Unbounded_Integer;
                     Right     : in     Unbounded_Integer;
                     Quotient  :    out Unbounded_Integer;
                     Remainder :    out Unbounded_Integer);
   -- Left > 0, Right > 0.

   function El_Or_0 (List : Digit_List; Index : Positive) return Digit_Value is
      -- Empty declarative part
   begin -- El_Or_0
      if Index in 1 .. List.Last_Index then
         return List.Element (Index);
      end if;

      return 0;
   end El_Or_0;

   procedure Insert (List : in out Digit_List; Index : in Positive; Value : in Digit_Value) is
      -- Empty declarative part
   begin -- Insert
      if Index in 1 .. List.Last_Index then
         List.Replace_Element (Index => Index, New_Item => Value);
      else
         if Index > List.Last_Index + 1 then
            List.Append (New_Item => 0, Count => Ada.Containers.Count_Type (Index - List.Last_Index - 1) );
         end if;

         List.Append (New_Item => Value);
      end if;
   end Insert;

   procedure Reduce_Digits (Value : in out Unbounded_Integer) is
      -- Empty declarative part
   begin -- Reduce_Digits
      Reduce : loop
         exit Reduce when Value.Digit.Last_Index = 1 or Value.Digit.Last_Element /= 0;

         Value.Digit.Delete_Last;
      end loop Reduce;
   end Reduce_Digits;

   function Single_Zero return Digit_List is
      Result : Digit_List;
   begin -- Single_Zero
      Result.Append (New_Item => 0);

      return Result;
   end Single_Zero;

   type Big is range System.Min_Int .. System.Max_Int;

   function To_Unbounded_Integer (Value : Integer) return Unbounded_Integer is
      Abs_Value : constant Calculation_Value := Calculation_Value (abs Big (Value) );

      Result : Unbounded_Integer;
   begin -- To_Unbounded_Integer
      Result.Digit.Replace_Element (Index => 1, New_Item => Digit_Value (Abs_Value rem Radix) );

      if Abs_Value >= Radix then
         Result.Digit.Append (New_Item => Digit_Value (Abs_Value / Radix) );
      end if;

      Result.Negative := Value < 0;

      return Result;
   end To_Unbounded_Integer;

   function To_Integer (Value : Unbounded_Integer) return Integer is
      Result : Calculation_Value := Calculation_Value (Value.Digit.First_Element);
   begin -- To_Integer
      if Value.Digit.Last_Index > 2 then
         raise Constraint_Error;
      end if;

      if Value.Digit.Last_Index = 2 then
         Result := Result + Radix * Calculation_Value (Value.Digit.Last_Element);
      end if;

      if Value.Negative then
         return Integer (-Big (Result) );
      end if;

      return Integer (Big (Result) );
   end To_Integer;

   Zip : constant Unbounded_Integer := To_Unbounded_Integer (0);
   One : constant Unbounded_Integer := To_Unbounded_Integer (1);

   function "+" (Right : Unbounded_Integer) return Unbounded_Integer is
      -- Empty declarative part
   begin -- "+"
      return Right;
   end "+";

   function "-" (Right : Unbounded_Integer) return Unbounded_Integer is
      Result : Unbounded_Integer := Right;
   begin -- "-"
      Result.Negative := not Result.Negative;

      return Result;
   end "-";

   function "abs" (Right : Unbounded_Integer) return Unbounded_Integer is
      Result : Unbounded_Integer := Right;
   begin -- "abs"
      Result.Negative := False;

      return Result;
   end "abs";

   function "+" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Max_MSD : constant Natural := Integer'Max (Left.Digit.Last_Index, Right.Digit.Last_Index);

      Result : Unbounded_Integer;
      Sum    : Calculation_Value := 0;
   begin -- "+"
      if Left.Negative /= Right.Negative then
         if Right.Negative then
            return Left - (abs Right);
         end if;

         return Right - (abs Left);
      end if;

      Add : for I in 1 .. Max_MSD loop
         Sum := Calculation_Value (El_Or_0 (Left.Digit, I) ) + Calculation_Value (El_Or_0 (Right.Digit, I) ) + Sum / Radix;
         Insert (List => Result.Digit, Index => I, Value => Digit_Value (Sum rem Radix) );
      end loop Add;

      Sum := Sum / Radix;

      if Sum /= 0 then -- Carry into final digit
         Insert (List => Result.Digit, Index => Max_MSD + 1, Value => Digit_Value (Sum) );
      end if;

      Result.Negative := Left.Negative;

      return Result;
   end "+";

   function "-" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Max_MSD : constant Natural := Integer'Max (Left.Digit.Last_Index, Right.Digit.Last_Index);

      Work_Left   : Unbounded_Integer;
      Work_Right  : Unbounded_Integer;
      Result      : Unbounded_Integer;
      Left_Digit  : Calculation_Value;
      Right_Digit : Calculation_Value;
      Borrow      : Boolean := False;
   begin -- "-"
      if Left = Right then
         return Zip;
      end if;

      if Left.Negative /= Right.Negative then
         if Left.Negative then
            return -(abs Left + Right); -- -X - Y = -(X + Y)
         end if;

         return Left + abs Right; -- X - (-Y) = X + Y
      end if;

      -- Signs are the same

      if abs Left > abs Right then --  Compute |Left| - |Right|, then Result has sign of Left
         Work_Left  := Left;
         Work_Right := Right;
         Result.Negative := Left.Negative;
      else -- Compute |Right| - |Left|, then Result has opposite sign to Left
         Work_Left  := Right;
         Work_Right := Left;
         Result.Negative := not Left.Negative;
      end if;

      Sub : for I in 1 .. Max_MSD loop
         Left_Digit := Calculation_Value (El_Or_0 (Work_Left.Digit, I) );

         if Borrow then -- We borrowed from this digit the previous time through the loop, so reduce it by 1
            if Left_Digit = 0 then -- We need to borrow from the next digit, too
               Left_Digit := Calculation_Value (Digit_Value'Last);
            else
               Left_Digit := Left_Digit - 1;
               Borrow := False;
            end if;
         end if;

         Right_Digit := Calculation_Value (El_Or_0 (Work_Right.Digit, I) );

         if Left_Digit < Right_Digit then -- We need to borrow from the next digit
            Left_Digit := Left_Digit + Radix;
            Borrow := True;
         end if;

         Insert (List => Result.Digit, Index => I, Value => Digit_Value (Left_Digit - Right_Digit) );
      end loop Sub;

      Reduce_Digits (Value => Result);

      return Result;
   end "-";

   function "*" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Result : Unbounded_Integer;
      D      : Calculation_Value;
      S      : Calculation_Value;
      K      : Positive;
   begin -- "*"
      if Left = Zip or Right = Zip then
         return Zip;
      end if;

      All_Left_Digits : for J in 1 .. Left.Digit.Last_Index loop
         D := 0;
         K := J;

         All_Right_Digits : for I in 1 .. Right.Digit.Last_Index + 1 loop
            D := D / Radix; -- Carry from the result of the multiplication of the previous pair of digits

            if I <= Right.Digit.Last_Index then
               D := D + Calculation_Value (Left.Digit.Element (J) ) * Calculation_Value (Right.Digit.Element (I) );
            end if; -- D now equals Left (J) * Right (I) + Carry

            S := Calculation_Value (El_Or_0 (Result.Digit, K) ) + D rem Radix; -- Add D to the evolving product
            Insert (List => Result.Digit, Index => K, Value => Digit_Value (S rem Radix) );
            K := K + 1;
            Insert (List => Result.Digit, Index => K, Value => El_Or_0 (Result.Digit, K) + Digit_Value (S / Radix) );
         end loop All_Right_Digits;
      end loop All_Left_Digits;

      Result.Negative := Left.Negative /= Right.Negative;
      Reduce_Digits (Value => Result);

      return Result;
   end "*";

   procedure Divide (Left      : in     Unbounded_Integer;
                     Right     : in     Unbounded_Integer;
                     Quotient  :    out Unbounded_Integer;
                     Remainder :    out Unbounded_Integer)
   is
      procedure Divide_Special (Left      : in     Unbounded_Integer;
                     		Right     : in     Unbounded_Integer;
                     		Quotient  :    out Unbounded_Integer;
                     		Remainder :    out Unbounded_Integer);
      -- For the case where Left.Digit.Last_Index in Right.Digit.Last_Index .. Right.Digit.Last_Index + 1

      procedure Shift_Left (Value : in out Unbounded_Integer; Places : in Natural);
      -- Deletes the first Places elements of Value.Digit

      procedure Drop_Significant (Value : in out Unbounded_Integer; Remaining : in Positive);
      -- Deletes the most significant digits from Value until there are Remaining digits left

      procedure Append (Onto : in out Unbounded_Integer; From : in Unbounded_Integer);
      -- Appends the digits of From onto the end of Onto
      -- Onto := Onto + Shift_Left (From, Onto.Digit.Last_index)

      procedure Divide_Special (Left      : in     Unbounded_Integer;
                     		Right     : in     Unbounded_Integer;
                     		Quotient  :    out Unbounded_Integer;
                     		Remainder :    out Unbounded_Integer)
      is
         Big_Radix   : Unbounded_Integer;
         Radix_Right : Unbounded_Integer;
         Left_Digit  : Calculation_Value;
         Right_Digit : Calculation_Value;
         Q           : Calculation_Value;
      begin -- Divide_Special
         Insert (List => Big_Radix.Digit, Index => 2, Value => 1); -- Big_Radix = Radix
         Radix_Right := Big_Radix * Right;

         if Left >= Radix_Right then
            Divide (Left => Left - Radix_Right, Right => Right, Quotient => Quotient, Remainder => Remainder);
            Quotient := Quotient + Big_Radix;

            return;
         end if;

         if Left.Digit.Last_Index = Right.Digit.Last_Index then
            Left_Digit := Calculation_Value (Left.Digit.Last_Element);
         else
            Left_Digit := Radix * Calculation_Value (Left.Digit.Last_Element) +
                          Calculation_Value (Left.Digit.Element (Left.Digit.Last_Index - 1) );
         end if;

         Right_Digit := Calculation_Value (Right.Digit.Last_Element);
         Q := Left_Digit / Right_Digit;

         if Q >= Radix then
            Q := Radix - 1;
         end if;

         Quotient := Zip;
         Insert (List => Quotient.Digit, Index => 1, Value => Digit_Value (Q) );
         Remainder := Quotient * Right;

         Reduce : loop -- Max 2 iterations
            exit Reduce when Remainder <= Left;

            Quotient  := Quotient - One;
            Remainder := Remainder - Right;
         end loop Reduce;

         Remainder := Left - Remainder;
      end Divide_Special;

      procedure Shift_Left (Value : in out Unbounded_Integer; Places : in Natural) is
         -- Empty declarative region
      begin -- Shift_Left
         Delete : for I in 1 .. Places loop
            exit Delete when Value.Digit.Last_Index = 0;

            Value.Digit.Delete_First;
         end loop Delete;

         if Value.Digit.Last_Index = 0 then
            Value := Zip;
         end if;
      end Shift_Left;

      procedure Drop_Significant (Value : in out Unbounded_Integer; Remaining : in Positive) is
         -- Empty declarative region
      begin -- Drop_Significant
         Delete : loop
            exit Delete when Value.Digit.Last_Index <= Remaining;

            Value.Digit.Delete_Last;
         end loop Delete;
      end Drop_Significant;

      procedure Append (Onto : in out Unbounded_Integer; From : in Unbounded_Integer) is
         -- Empty declarative region
      begin -- Append
         Append_Digits : for I in 1 .. From.Digit.Last_Index loop
            Onto.Digit.Append (New_Item => From.Digit.Element (I) );
         end loop Append_Digits;
      end Append;

      Abs_Left    : Unbounded_Integer := Left;
      Abs_Right   : Unbounded_Integer := Right;
      Left_Digit  : Calculation_Value;
      Right_Digit : Calculation_Value;
      Left_P      : Unbounded_Integer; -- A'
      S           : Unbounded_Integer;
      Quotient_P  : Unbounded_Integer; -- q'
      Remainder_P : Unbounded_Integer; -- r'
   begin -- Divide
      if Right = Zip then
         raise Constraint_Error with "Division by zero";
      end if;

      if Left < Right then
         Quotient := Zip;
         Remainder := Left;

         return;
      end if;

      if Right = One then
         Quotient := Left;
         Remainder := Zip;

         return;
      end if;

      Reduce : loop -- Move the point left until one of the operands is not a multiple of Radix: 1000 / 50 = 100 / 5
         exit Reduce when Abs_Left.Digit.First_Element /= 0 or Abs_Right.Digit.First_Element /= 0;

         Abs_Left.Digit.Delete_First;
         Abs_Right.Digit.Delete_First;
      end loop Reduce;

      if Abs_Right = One then
         Quotient := Abs_Left;
         Remainder := Zip;

         return;
      end if;

      if Abs_Left.Digit.Last_Index <= 2 and Abs_Right.Digit.Last_Index <= 2 then -- Both values will fit in Calculation_Value
         Left_Digit := Calculation_Value (Abs_Left.Digit.First_Element);

         if Abs_Left.Digit.Last_Index > 1 then
            Left_Digit := Left_Digit + Calculation_Value (Abs_Left.Digit.Element (2) ) * Radix;
         end if;

         Right_Digit := Calculation_Value (Abs_Right.Digit.First_Element);

         if Abs_Right.Digit.Last_Index > 1 then
            Right_Digit := Right_Digit + Calculation_Value (Abs_Right.Digit.Element (2) ) * Radix;
         end if;

         Build_Results : declare
            Result : constant Calculation_Value := Left_Digit / Right_Digit;
            Remain : constant Calculation_Value := Left_Digit - Result * Right_Digit;
         begin -- Build_Results
            Insert (List => Quotient.Digit, Index => 1, Value => Digit_Value (Result rem Radix) );
            Insert (List => Quotient.Digit, Index => 2, Value => Digit_Value (Result / Radix) );

            Insert (List => Remainder.Digit, Index => 1, Value => Digit_Value (Remain rem Radix) );
            Insert (List => Remainder.Digit, Index => 2, Value => Digit_Value (Remain / Radix) );

            Reduce_Digits (Value => Quotient);
            Reduce_Digits (Value => Remainder);

            return;
         end Build_Results;
      end if;

      -- This is called "schoolbook division", and is supposed to be a good choice for numbers < about 800 bits, which covers
      -- most of my needs.

      -- Right must be >= Radix / 2; multiply both operands by a factor to ensure this if necessary
      if Abs_Right.Digit.Last_Index = 1 then
         Adjust : declare
            Factor_Digit : Calculation_Value := Calculation_Value (Abs_Right.Digit.First_Element);
            Factor       : Unbounded_Integer;
         begin -- Adjust
            if Factor_Digit < Radix / 2 then
               Factor_Digit := (Radix / 2) / Factor_Digit + 1;
               Insert (List => Factor.Digit, Index => 1, Value => Digit_Value (Factor_Digit rem Radix) );
               Insert (List => Factor.Digit, Index => 2, Value => Digit_Value (Factor_Digit / Radix) );
               Reduce_Digits (Value => Factor);
               Abs_Left := Factor * Abs_Left;
               Abs_Right := Factor * Abs_Right;
            end if;
         end Adjust;
      end if;

      if Abs_Left.Digit.Last_Index in Abs_Right.Digit.Last_Index .. Abs_Right.Digit.Last_Index + 1 then
         Divide_Special (Left => Abs_Left, Right => Abs_Right, Quotient => Quotient, Remainder => Remainder);

         return;
      end if;

      Left_P := Abs_Left;
      Shift_Left (Value => Left_P, Places => Abs_Left.Digit.Last_Index - Abs_Right.Digit.Last_Index - 1);
      S := Abs_Left;
      Drop_Significant (Value => S, Remaining => Abs_Left.Digit.Last_Index - Abs_Right.Digit.Last_Index - 1);
      -- Left_P is the Abs_Right.Digit.Last_Index + 1 most significant digits of Abs_Left
      -- S is the remaining least significant digits of Abs_Left

      Divide_Special (Left => Left_P, Right => Abs_Right, Quotient => Quotient_P, Remainder => Remainder_P);

      -- S := S + Shift_Left (Remainder_P, S.Digit.Last_Index)
      Append (Onto => S, From => Remainder_P);

      Divide (Left => S, Right => Abs_Right, Quotient => Quotient, Remainder => Remainder);

      -- Quotient := Quotient + Shift_Left (Quotient_P, Quotient.Digit.Last_Index)
      Append (Onto => Quotient, From => Quotient_P);
   end Divide;

   function "/" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Quotient  : Unbounded_Integer;
      Remainder : Unbounded_Integer;
   begin -- "/"
      Divide (Left => abs Left, Right => abs Right, Quotient => Quotient, Remainder => Remainder);

      Quotient.Negative := Left.Negative /= Right.Negative;

      return Quotient;
   end "/";

   function "rem" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Quotient  : Unbounded_Integer;
      Remainder : Unbounded_Integer;
   begin -- "rem"
      Divide (Left => abs Left, Right => abs Right, Quotient => Quotient, Remainder => Remainder);

      Remainder.Negative := Left.Negative;

      return Remainder;
   end "rem";

   function "mod" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Remainder : constant Unbounded_Integer := Left rem Right;
   begin -- "mod"
      if Remainder = Zip or Right.Negative = Remainder.Negative then
         return Remainder;
      end if;

      return Right + Remainder;
   end "mod";

   function "**" (Left : Unbounded_Integer; Right : Natural) return Unbounded_Integer is
      Result : Unbounded_Integer := Left;
      Work   : Unbounded_Integer := Left;
   begin -- "**"
      if Right = 0 then
         return One;
      end if;

      if Right = 1 then
         return Left;
      end if;

      if Left = Zip then
         return Zip;
      end if;

      Calculate : declare -- This is O(log Right)
         Power : Natural := Right - 1;
      begin -- Calculate
         Multiply : loop
            exit Multiply when Power = 0;

            if Power rem 2 = 0 then -- X ** (2 * P) = (X ** 2) ** P
               Work := Work * Work;
               Power := Power / 2;
            else
               Result := Work * Result;
               Power := Power - 1;
            end if;
         end loop Multiply;
      end Calculate;

      return Result;
   end "**";

   function "=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      -- Empty declarative part
   begin -- "="
      if Left.Digit.Last_Index = 1 and then Left.Digit.Element (1) = 0 then
         return Right.Digit.Last_Index = 1 and then Right.Digit.Element (1) = 0; -- Zero = Zero, regardless of sign
      end if;

      if Left.Negative /= Right.Negative then
         return False;
      end if;

      return Left.Digit = Right.Digit;
   end "=";

   function ">" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      -- Empty declarative part
   begin -- ">"
      if Left.Negative then
         if not Right.Negative then
            return False;
         end if;

         if Left.Digit.Last_Index < Right.Digit.Last_Index then
            return True;
         end if;

         if Left.Digit.Last_Index > Right.Digit.Last_Index then
            return False;
         end if;
      else
         if Right.Negative then
            return True;
         end if;

         if Left.Digit.Last_Index > Right.Digit.Last_Index then
            return True;
         end if;

         if Left.Digit.Last_Index < Right.Digit.Last_Index then
            return False;
         end if;
      end if;

      -- Signs are the same and both have the same number of digits

      Compare_Digits : declare
         Work_Left   : Unbounded_Integer;
         Work_Right  : Unbounded_Integer;
         Left_Digit  : Digit_Value;
         Right_Digit : Digit_Value;
      begin -- Compare_Digits
         if Left.Negative then -- Left > Right = |Right| > |Left|
            Work_Left  := Right;
            Work_Right := Left;
         else
            Work_Left  := Left;
            Work_Right := Right;
         end if;

         Compare : for I in reverse 1 .. Work_Left.Digit.Last_Index loop
            Left_Digit  := Work_Left.Digit.Element  (I);
            Right_Digit := Work_Right.Digit.Element (I);

            if Left_Digit /= Right_Digit then -- Most significant unequal digit determines the result
               return Left_Digit > Right_Digit;
            end if;
         end loop Compare;

         return False; -- All digits the same; Left = Right
      end Compare_Digits;
   end ">";

   function "<" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      -- Empty declarative part
   begin -- "<"
      return Right > Left;
   end "<";

   function ">=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      -- Empty declarative part
   begin -- ">="
      return not (Right > Left);
   end ">=";

   function "<=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      -- Empty declarative part
   begin -- "<="
      return not (Left > Right);
   end "<=";

   function Image (Value : Unbounded_Integer; Base : Base_Number := 10; Decorated : Boolean := False) return String is
      function Decoration (Result : Ada.Strings.Unbounded.Unbounded_String; Negative : Boolean; Base : Base_Number)
      return String;
      -- Returns Result with decorations for Negative and Base

      function Decoration (Result : Ada.Strings.Unbounded.Unbounded_String; Negative : Boolean; Base : Base_Number)
      return String is
         Base_Image : constant String := Base_Number'Image (Base);
      begin -- Decoration
         if Negative then
            return '-' & Base_Image (Base_Image'First + 1 .. Base_Image'Last) & '#' &
                   Ada.Strings.Unbounded.To_String (Result) & '#';
         end if;

         return Base_Image (Base_Image'First + 1 .. Base_Image'Last) & '#' &
                Ada.Strings.Unbounded.To_String (Result) & '#';
      end Decoration;

      Last : constant := Base_Number'Last - 1;

      subtype Base_Value   is Digit_Value range 0 .. Last;
      subtype Digit_String is String (1 .. 1);

      type Conversion_Map is array (Base_Value) of Digit_String;

      Letter : constant Conversion_Map := ( 0 => "0",  1 => "1",  2 => "2",  3 => "3",  4 => "4",  5 => "5",
                                            6 => "6",  7 => "7",  8 => "8",  9 => "9", 10 => "A", 11 => "B",
                                           12 => "C", 13 => "D", 14 => "E", 15 => "F", 16 => "G", 17 => "H",
                                           18 => "I", 19 => "J", 20 => "K", 21 => "L", 22 => "M", 23 => "N",
                                           24 => "O", 25 => "P", 26 => "Q", 27 => "R", 28 => "S", 29 => "T",
                                           30 => "U", 31 => "V", 32 => "W", 33 => "X", 34 => "Y", 35 => "Z");

      U_Base : constant Unbounded_Integer := To_Unbounded_Integer (Integer (Base) );

      Result : Ada.Strings.Unbounded.Unbounded_String;
      Digit  : Unbounded_Integer;
      Work   : Unbounded_Integer := abs Value;
      Temp   : Unbounded_Integer;
   begin -- Image
      if Value = Zip then
         if Decorated then
            return Decoration (Ada.Strings.Unbounded.To_Unbounded_String (Letter (0) ), False, Base);
         else
            return Letter (0);
         end if;
      end if;

      All_Digits : loop
         exit All_Digits when Work = Zip;

         Temp := Work / U_Base;
         Digit := Work - U_Base * Temp; -- Digit := Work rem U_Base; -- This saves a division since we need both div and rem
         Work := Temp;
         Ada.Strings.Unbounded.Insert (Source => Result, Before => 1, New_Item => Letter (Digit.Digit.First_Element) );
      end loop All_Digits;

      if Decorated then
         return Decoration (Result, Value.Negative, Base);
      end if;

      if Value.Negative then
         return '-' & Ada.Strings.Unbounded.To_String (Result);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   function Value (Image : String) return Unbounded_Integer is
      Work : constant String := Ada.Characters.Handling.To_Upper (Ada.Strings.Fixed.Trim (Image, Ada.Strings.Both) );

      subtype Digit_Range is Character range '0' .. '9';

      type Digit_Map is array (Digit_Range) of Natural;

      Digit : constant Digit_Map :=
         ('0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9);

      subtype Upper_Case is Character range 'A' .. 'Z';

      type Letter_Map is array (Upper_Case) of Natural;

      Letter : constant Letter_Map := ('A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15, 'G' => 16, 'H' => 17,
                                       'I' => 18, 'J' => 19, 'K' => 20, 'L' => 21, 'M' => 22, 'N' => 23, 'O' => 24, 'P' => 25,
                                       'Q' => 26, 'R' => 27, 'S' => 28, 'T' => 29, 'U' => 30, 'V' => 31, 'W' => 32, 'X' => 33,
                                       'Y' => 34, 'Z' => 35);

      Start : constant Positive := Ada.Strings.Fixed.Index (Work, "#") + 1;
      Based : constant Boolean  := Start > Work'First;
      Stop  : constant Positive := Work'Last - Boolean'Pos (Based);

      Base   : Unbounded_Integer;
      Result : Unbounded_Integer;
   begin -- Value
      if Based then
         Base := To_Unbounded_Integer (Integer'Value (Work (Work'First .. Start - 2) ) );
      else
         Base := To_Unbounded_Integer (10);
      end if;

      All_Digits : for I in Start .. Stop loop
         case Work (I) is
         when Digit_Range =>
            Result := Base * Result + To_Unbounded_Integer (Digit (Work (I) ) );
         when Upper_Case =>
            Result := Base * Result + To_Unbounded_Integer (Letter (Work (I) ) );
         when others =>
            raise Constraint_Error with "Invalid character in Image";
         end case;
      end loop All_Digits;

      return Result;
   end Value;

   function GCD (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      Min       : Unbounded_Integer := abs Left;
      Max       : Unbounded_Integer := abs Right;
      Remainder : Unbounded_Integer;
   begin -- GCD
      if Max < Min then
         Remainder := Min;
         Min := Max;
         Max := Remainder;
      end if; -- Now Min <= Max

      if Min = Zip and Max = Zip then
         raise Constraint_Error with "GCD (0, 0)";
      end if;

      if Min = Max then
         return Min;
      end if;

      Reduce : loop
         if Min <= Zip then
            return Max;
         end if;

         Remainder := Max rem Min;
         Max := Min;
         Min := Remainder;
      end loop Reduce;
   end GCD;

   function LCM (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer is
      -- Empty declarative part
   begin -- LCM
      return (Left * Right) / GCD (Left, Right);
   end LCM;
end PragmARC.Unbounded_Integers;
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
