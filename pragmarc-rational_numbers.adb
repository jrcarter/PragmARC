-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2017 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Rational numbers bounded only by Integer'Last and available memory
--
-- History:
-- 2017 Apr 15     J. Carter          V1.1--Removed GCD and LCM (now in Unbounded_Integers) and added Sqrt
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body PragmARC.Rational_Numbers is
   procedure Simplify (Value : in out Rational);
   -- Changes Value to have the smallest (absolute) values that represent the same rational number
   -- 2/4 becomes 1/2

   function Compose
      (Numerator : PragmARC.Unbounded_Integers.Unbounded_Integer; Denominator : PragmARC.Unbounded_Integers.Unbounded_Integer)
   return Rational is
      Result : Rational := (Numerator => Numerator, Denominator => Denominator);
   begin -- Compose
      if Numerator < UI0 then
         if Denominator < UI0 then
            Result := (Numerator => abs Numerator, Denominator => abs Denominator);
         end if;
         -- Else signs are OK
      elsif Denominator < UI0 then
         if Numerator > UI0 then
            Result := (Numerator => -Numerator, Denominator => abs Denominator);
         end if;
         -- Else Numerator is zero and Simplify will adjust the denominator
      else
         null; -- Signs are OK
      end if;

      Simplify (Value => Result);

      return Result;
   end Compose;

   procedure Decompose (Value       : in     Rational;
                        Numerator   :    out PragmARC.Unbounded_Integers.Unbounded_Integer;
                        Denominator :    out PragmARC.Unbounded_Integers.Unbounded_Integer)
   is
      -- Empty declarative part
   begin -- Decompose
      Numerator := Value.Numerator;
      Denominator := Value.Denominator;
   end Decompose;

   function "+" (Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "+"
      return Right;
   end "+";

   function "-" (Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "-"
      return (Numerator => -Right.Numerator, Denominator => Right.Denominator);
   end "-";

   function "abs" (Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "abs"
      return (Numerator => abs Right.Numerator, Denominator => Right.Denominator);
   end "abs";

   function "+" (Left : Rational; Right : Rational) return Rational is
      M  : Unbounded_Integer;
      LN : Unbounded_Integer;
      RN : Unbounded_Integer;
   begin -- "+"
      if Left.Denominator = Right.Denominator then
         return Compose (Left.Numerator + Right.Numerator, Left.Denominator);
      end if;

      if Left.Denominator = UI1 then
         return Compose (Left.Numerator * Right.Denominator + Right.Numerator, Right.Denominator);
      end if;

      if Right.Denominator = UI1 then
         return Compose (Left.Numerator + Right.Numerator * Left.Denominator, Left.Denominator);
      end if;

      M := LCM (abs Left.Denominator, abs Right.Denominator);

      if M = Left.Denominator then
         LN := Left.Numerator;
      else
         LN := Left.Numerator  * M / Left.Denominator;
      end if;

      if M = Right.Denominator then
         RN := Right.Numerator;
      else
         RN := Right.Numerator * M / Right.Denominator;
      end if;

      return Compose (LN + RN, M);
   end "+";

   function "-" (Left : Rational; Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "-"
      return Left + (-Right);
   end "-";

   function "*" (Left : Rational; Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "*"
      return Compose (Left.Numerator * Right.Numerator, Left.Denominator * Right.Denominator);
   end "*";

   function "/" (Left : Rational; Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "/"
      if Right = Zero then
         raise Constraint_Error with "Division by zero";
      end if;

      if Right < Zero then
         return Compose (Left.Numerator * (-Right.Denominator), Left.Denominator * (abs Right.Numerator) );
      end if;

      return Compose (Left.Numerator * Right.Denominator, Left.Denominator * Right.Numerator);
   end "/";

   function "**" (Left : Rational; Right : Integer) return Rational is
      Result : Rational := Left;
      Work   : Rational := Left;
   begin -- "**"`
      if Right = 0 then
         return One;
      end if;

      if Right = 1 then
         return Left;
      end if;

      if Left = Zero then
         return Zero;
      end if;

      if Right < 0 then
         return One / Left ** (abs Right);
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

   function ">"  (Left : Rational; Right : Rational) return Boolean is
      M : Unbounded_Integer;
   begin -- ">"
      if Left.Denominator = Right.Denominator then
         return Left.Numerator > Right.Numerator;
      end if;

      if Left.Numerator < UI0 then
         if Right.Numerator >= UI0 then
            return False;
         end if;
      elsif Right.Numerator < UI0 then
         return True;
      else
         null;
      end if;

       -- Signs are the same

      M := LCM (abs Left.Denominator, abs Right.Denominator);

      return Unbounded_Integer'(Left.Numerator * M / Left.Denominator) > Right.Numerator * M / Right.Denominator;
   end ">";

   function "<"  (Left : Rational; Right : Rational) return Boolean is
      -- Empty declarative part
   begin -- "<"
      return Right > Left;
   end "<";

   function ">=" (Left : Rational; Right : Rational) return Boolean is
      -- Empty declarative part
   begin -- ">="
      return not (Right > Left);
   end ">=";

   function "<=" (Left : Rational; Right : Rational) return Boolean is
      -- Empty declarative part
   begin -- "<="
      return not (Left > Right);
   end "<=";

   function Image (Value : Rational; As_Fraction : Boolean := False; Base : Base_Number := 10; Decorated : Boolean := False)
   return String is
      Ten : constant Unbounded_Integer := To_Unbounded_Integer (10);

      Work   : Unbounded_Integer := abs Value.Numerator;
      Q      : Unbounded_Integer;
      Result : Ada.Strings.Unbounded.Unbounded_String;

      use Ada.Strings.Unbounded;
   begin -- Image
      if As_Fraction then
         return Image (Value.Numerator,   Unbounded_Integers.Base_Number (Base), Decorated) & '/' &
                Image (Value.Denominator, Unbounded_Integers.Base_Number (Base), Decorated);
      end if;

      if Value.Numerator < UI0 then
         Append (Source => Result, New_Item => '-');
      end if;

      Q := Work / Value.Denominator;

      Append (Source => Result, New_Item => Image (Q) & '.');

      Work := Work - Q * Value.Denominator;

      if Work = UI0 then
         return To_String (Result) & '0';
      end if;

      Zeros : loop
         exit Zeros when Q /= UI0;

         Work := Ten * Work;
         Q := Work / Value.Denominator;
         Append (Source => Result, New_Item => Image (Q) );
         Work := Work - Q * Value.Denominator;
      end loop Zeros;

      Count : for I in 1 .. 1_000 loop
         exit Count when Work = UI0;

         Work := Ten * Work;
         Q := Work / Value.Denominator;
         Append (Source => Result, New_Item => Image (Q) );
         Work := Work - Q * Value.Denominator;
      end loop Count;

      return To_String (Result);
   end Image;

   function Value (Image : String) return Rational is
      Slash : constant Natural := Ada.Strings.Fixed.Index (Image, "/");
      Dot   : constant Natural := Ada.Strings.Fixed.Index (Image, ".");
      Hash  : constant Natural := Ada.Strings.Fixed.Index (Image, "#");
   begin -- Value
      if Slash > 0 then
         return Compose (Value (Image (Image'First .. Slash - 1) ), Value (Image (Slash + 1 .. Image'Last) ) );
      end if;

      if Dot = 0 then
         return (Numerator => Value (Image), Denominator => UI1);
      end if;

      if Dot = Image'Last then
         return (Numerator => Value (Image (Image'First .. Image'Last - 1) ), Denominator => UI1);
      end if;

      if Hash = 0 then
         return Compose (Value (Image (Image'First .. Dot - 1) & Image (Dot + 1 .. Image'Last) ),
                         Value ('1' & (1 .. Image'Last - Dot => '0') ) );
      end if;

      return Compose (Value (Image (Image'First .. Dot - 1) & Image (Dot + 1 .. Image'Last) ),
                      Value (Image (Image'First .. Hash) & '1' & (1 .. Image'Last - Dot - 1 => '0') & '#') );
   end Value;

   procedure Simplify (Value : in out Rational) is
      D : Unbounded_Integer;
   begin -- Simplify
      if Value.Numerator = UI0 then
         if Value.Denominator = UI0 then
            raise Constraint_Error with "Division by zero";
         end if;

         Value := Zero;

         return;
      end if;

      D := GCD (Value.Numerator, Value.Denominator);

      Value := (Numerator => Value.Numerator / D, Denominator => Value.Denominator / D);
   end Simplify;

   Two : constant Rational := One + One;

   type Square_Pair is record
      Number : Rational;
      Square : Rational;
   end record;

   type Square_List is array (Positive range <>) of Square_Pair;

   Square : constant Square_List := ( (Number => Value (     "2.0"), Square => Value (           "4.0") ),
                                      (Number => Value (     "4.0"), Square => Value (          "16.0") ),
                                      (Number => Value (     "8.0"), Square => Value (          "64.0") ),
                                      (Number => Value (    "16.0"), Square => Value (         "256.0") ),
                                      (Number => Value (    "32.0"), Square => Value (        "1024.0") ),
                                      (Number => Value (    "64.0"), Square => Value (        "4096.0") ),
                                      (Number => Value (   "128.0"), Square => Value (       "16384.0") ),
                                      (Number => Value (   "256.0"), Square => Value (       "65536.0") ),
                                      (Number => Value (   "512.0"), Square => Value (      "262144.0") ),
                                      (Number => Value (  "1024.0"), Square => Value (     "1048576.0") ),
                                      (Number => Value (  "2048.0"), Square => Value (     "4194304.0") ),
                                      (Number => Value (  "4096.0"), Square => Value (    "16777216.0") ),
                                      (Number => Value (  "8192.0"), Square => Value (    "67108864.0") ),
                                      (Number => Value ( "16384.0"), Square => Value (   "268435456.0") ),
                                      (Number => Value ( "32768.0"), Square => Value (  "1073741824.0") ),
                                      (Number => Value ( "65536.0"), Square => Value (  "4294967296.0") ),
                                      (Number => Value ("131072.0"), Square => Value ( "17179869184.0") ),
                                      (Number => Value ("262144.0"), Square => Value ( "68719476736.0") ),
                                      (Number => Value ("524288.0"), Square => Value ("274877906944.0") ) );

   function Sqrt (Right : Rational; Accuracy : Rational) return Rational is
      A : constant Rational := abs Accuracy;

      R : Rational := Right; -- Right after reduction
      F : Rational := One;   -- Factor after reduction
      X : Rational;
      Y : Rational;
   begin -- Sqrt
      if Right < Zero then
         raise Constraint_Error with "Sqrt: Right < 0";
      end if;

      if Right = Zero then
         return Zero;
      end if;

      if Right = One then
         return One;
      end if;

      Reduce_All : for I in reverse Square'Range loop
         Reduce_One : loop
            exit Reduce_One when R < Square (I).Square;

            F := F * Square (I).Number;
            R := R / Square (I).Square;
         end loop Reduce_One;
      end loop Reduce_All;

      if R = One then
         return F;
      end if;

      X := R / Two;

      All_Iterations : for I in 1 .. 15 loop
         Y := X ** 2 - R;

         exit All_Iterations when abs Y < A;

         X := (X + R / X) / Two;
      end loop All_Iterations;

      return F * X;
   end Sqrt;
end PragmARC.Rational_Numbers;
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
