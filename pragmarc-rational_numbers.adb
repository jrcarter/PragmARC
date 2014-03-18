-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Rational numbers bounded only by Integer'Last and available memory
--
-- History:
-- 2014 Apr 01     J. Carter          V1.0--Initial release
--
with Ada.Strings.Fixed;

package body PragmARC.Rational_Numbers is
   function GCD (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;
   -- Greatest common divisor; signs are ignored

   function LCM (Left : Unbounded_Integer; Right : Unbounded_Integer) return Unbounded_Integer;
   -- Least common multiple; signs are ignored

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
      M      : Unbounded_Integer;
      LN     : Unbounded_Integer;
      RN     : Unbounded_Integer;
      Result : Rational;
   begin -- "+"
      if Left.Denominator = Right.Denominator then
         return (Numerator => Left.Numerator + Right.Numerator, Denominator => Left.Denominator);
      end if;

      M := LCM (Left.Denominator, Right.Denominator);
      LN := Left.Numerator  * M / Left.Denominator;
      RN := Right.Numerator * M / Right.Denominator;
      Result := (Numerator => LN + RN, Denominator => M);
      Simplify (Value => Result);

      return Result;
   end "+";

   function "-" (Left : Rational; Right : Rational) return Rational is
      -- Empty declarative part
   begin -- "-"
      return Left + (-Right);
   end "-";

   function "*" (Left : Rational; Right : Rational) return Rational is
      Result : Rational := (Numerator => Left.Numerator * Right.Numerator, Denominator => Left.Denominator * Right.Denominator);
   begin -- "*"
      Simplify (Value => Result);

      return Result;
   end "*";

   function "/" (Left : Rational; Right : Rational) return Rational is
      Result : Rational;
   begin -- "/"
      if Right = Zero then
         raise Constraint_Error with "Division by zero";
      end if;

      if Right < Zero then
         Result := (Numerator => Left.Numerator * (-Right.Denominator), Denominator => Left.Denominator * (abs Right.Numerator) );
      else
         Result := (Numerator => Left.Numerator * Right.Denominator, Denominator => Left.Denominator * Right.Numerator);
      end if;

      Simplify (Value => Result);

      return Result;
   end "/";

   function "**" (Left : Rational; Right : Natural) return Rational is
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
      L : Rational;
      R : Rational;
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

      M := LCM (Left.Denominator, Right.Denominator);
      L := (Numerator => Left.Numerator  * M / Left.Denominator,  Denominator => M);
      R := (Numerator => Right.Numerator * M / Right.Denominator, Denominator => M);

      return L.Numerator > R.Numerator;
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

   function Image (Value : Rational; Base : Base_Number := 10; Decorated : Boolean := False) return String is
      -- Empty declarative part
   begin -- Image
      return Image (Value.Numerator,   Unbounded_Integers.Base_Number (Base), Decorated) & '/' &
             Image (Value.Denominator, Unbounded_Integers.Base_Number (Base), Decorated);
   end Image;

   function Value (Image : String) return Rational is
      Slash : constant Natural := Ada.Strings.Fixed.Index (Image, "/");
      Dot   : constant Natural := Ada.Strings.Fixed.Index (Image, ".");
      Hash  : constant Natural := Ada.Strings.Fixed.Index (Image, "#");

      Result : Rational;
   begin -- Value
      if Slash > 0 then
         Result := (Numerator   => Value (Image (Image'First .. Slash - 1) ),
                    Denominator => Value (Image (Slash + 1 .. Image'Last) ) );
         Simplify (Value => Result);

         return Result;
      end if;

      if Dot = 0 then
         return (Numerator => Value (Image), Denominator => UI1);
      end if;

      if Dot = Image'Last then
         return (Numerator => Value (Image (Image'First .. Image'Last - 1) ), Denominator => UI1);
      end if;

      if Hash = 0 then
         Result := (Numerator   => Value (Image (Image'First .. Dot - 1) & Image (Dot + 1 .. Image'Last) ),
                    Denominator => Value ('1' & (1 .. Image'Last - Dot => '0') ) );
      else
         Result := (Numerator   => Value (Image (Image'First .. Dot - 1) & Image (Dot + 1 .. Image'Last) ),
                    Denominator => Value (Image (Image'First .. Hash) & '1' & (1 .. Image'Last - Dot - 1 => '0') & '#') );
      end if;

      Simplify (Value => Result);

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

      Reduce : loop
         if Min <= UI0 then
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
      return (abs Left * (abs Right) ) / GCD (Left, Right);
   end LCM;

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
