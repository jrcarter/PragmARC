-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Sep 01     J. Carter          V1.5--Improve division
-- 2018 Oct 15     J. Carter          V1.4--Faster multiplication algorithm from Doug Rogers
-- 2018 Oct 01     J. Carter          V1.3--Handle negative numbers in Value
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2017 Apr 15     J. Carter          V1.1--Added GCD and LCM
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body PragmARC.Unbounded_Numbers.Integers is
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
      (if Index <= List.Last_Index Then List.Element (Index) else 0);

   procedure Insert (List : in out Digit_List; Index : in Positive; Value : in Digit_Value) is
      -- Empty declarative part
   begin -- Insert
      if Index <= List.Last_Index then
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

   function To_Unbounded_Integer (Value : Convertible_Integer) return Unbounded_Integer is
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
      (Right);

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
      L_Max : constant Positive := Left.Digit.Last_Index;
      R_Max : constant Positive := Right.Digit.Last_Index;

      Result   : Unbounded_Integer;
      Sum      : Calculation_Value := 0;
      Prev_Sum : Calculation_Value := 0;
      Carry    : Calculation_Value := 0;
      L_Start  : Natural           := 0;
      L_Stop   : Natural           := 0;

      function Get (From : Unbounded_Integer; Column : Natural) return Calculation_Value is
         (Calculation_Value (From.Digit.Element (Column + 1) ) );

      procedure Set (Into : in out Unbounded_Integer; Column : Natural; Value : Calculation_Value) is
         -- Empty declarative part
      begin
         Into.Digit.Replace_Element (Index => Column + 1, New_Item => Digit_Value (Value rem Radix) );
      end Set;
   begin -- "*"
      if Left = Zip or Right = Zip then
         return Zip;
      end if;

      Result.Negative := Left.Negative /= Right.Negative;
      Result.Digit.Set_Length (Length => Ada.Containers.Count_Type (L_Max + R_Max) );

      -- This calculates each digit of the result in turn from the digit products that contribute to it
      All_Result_Digits: for K in 0 .. L_Max + R_Max - 2 loop -- These are column numbers, starting at zero
         if K < R_Max then
            L_Start := 0;
         else
            L_Start := L_Start + 1;
         end if;

         if K < L_Max then
            L_Stop := K;
         else
            L_Stop := L_Max - 1;
         end if;

         Sum_For_K: for L in L_Start .. L_Stop loop
            Prev_Sum := Sum;
            Sum := Sum + Get (Left, L) * Get (Right, K - L);

            if Sum < Prev_Sum then
               Carry := Carry + Radix;
            end if;
         end loop Sum_For_K;

         Set (Into => Result, Column => K, Value => Sum);
         Sum := Sum / Radix + Carry;
         Carry := 0;
      end loop All_Result_Digits;

      Set (Into => Result, Column => L_Max + R_Max - 1, Value => Sum);
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
      S.Digit.Append (New_Item => Remainder_P.Digit);

      Divide (Left => S, Right => Abs_Right, Quotient => Quotient, Remainder => Remainder);

      -- Quotient := Quotient + Shift_Left (Quotient_P, Quotient.Digit.Last_Index)
      Quotient.Digit.Append (New_Item => Quotient_P.Digit);
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
      (Right > Left);

   function ">=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      (not (Right > Left) );

   function "<=" (Left : Unbounded_Integer; Right : Unbounded_Integer) return Boolean is
      (not (Left > Right) );

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
         Digit := Work - Temp * U_Base;
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

      Start : Positive := Ada.Strings.Fixed.Index (Work, "#") + 1;

      Based : constant Boolean  := Start > Work'First;
      Stop  : constant Positive := Work'Last - Boolean'Pos (Based);

      First  : Positive := Work'First;
      Base   : Unbounded_Integer;
      Result : Unbounded_Integer;
   begin -- Value
      if Work (First) = '-' then
         First := First + 1;

         if not Based then
            Start := Start + 1;
         end if;
      end if;

      if Based then
         Base := To_Unbounded_Integer (Integer'Value (Work (First .. Start - 2) ) );
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
            raise Constraint_Error with "Invalid character in Image: " & Work (I);
         end case;
      end loop All_Digits;

      Result.Negative := Work (Work'First) = '-'; -- We can't do this before because multiplication above will lose it

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
      ( (Left * Right) / GCD (Left, Right) );
end PragmARC.Unbounded_Numbers.Integers;
