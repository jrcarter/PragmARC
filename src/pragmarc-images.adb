-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Aug 15     J. Carter          V1.3--Added Base to Float_Image
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2006 Mar 01     J. Carter          V1.1--Added Float_Image
-- 2004 Apr 01     J. Carter          V1.0--Initial version
--
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Unbounded_Numbers.Integers;

package body PragmARC.Images is
   use Ada.Strings.Fixed;
   use Ada.Text_IO;

   function Adjust (Image : String; Width : Field; Negative : Boolean; Zero_Filled : Boolean) return String is
   -- Apply Width, Negative, & Zero_Filled to Image
      Blank : constant Character := ' ';
      Zero  : constant Character := '0';
      Minus : constant Character := '-';
   begin -- Adjust
      if Zero_Filled then
         if Negative then
            return Minus & (1 .. Width - Image'Length - 1 => Zero) & Image;
         else
            return (1 .. Width - Image'Length => Zero) & Image;
         end if;
      else
         if Negative then
            return (1 .. Width - Image'Length - 1 => Blank) & Minus & Image;
         else
            return (1 .. Width - Image'Length => Blank) & Image;
         end if;
      end if;
   end Adjust;

   function Signed_Image (Value : Number; Width : Field := 0; Zero_Filled : Boolean := False; Base : Number_Base := 10)
   return String is
      package Number_IO is new Integer_IO (Number);
      use Number_IO;

      Image : String (1 .. 100);
      Start : Positive;
      Stop  : Positive;

      Negative : constant Boolean := Value < 0;
   begin -- Signed_Image
      Put (To => Image, Item => Value, Base => Base);

      case Base is
      when 10 =>
         Start := Index_Non_Blank (Image);
         Stop  := Image'Last;

         if Negative then
            Start := Start + 1;
         end if;
      when 2 .. 9 | 11 .. 16 =>
         Start := 1 + Index (Image, "#");
         Stop  := Image'Last - 1;
      end case;

      return Adjust (Image (Start .. Stop), Width, Negative, Zero_Filled);
   end Signed_Image;

   function Modular_Image (Value : Number; Width : Field := 0; Zero_Filled : Boolean := False; Base : Number_Base := 10)
   return String is
      package Number_IO is new Modular_IO (Number);
      use Number_IO;

      Image : String (1 .. 100);
      Start : Positive;
      Stop  : Positive;
   begin -- Modular_Image
      Put (To => Image, Item => Value, Base => Base);

      case Base is
      when 10 =>
         Start := Index_Non_Blank (Image);
         Stop  := Image'Last;
      when 2 .. 9 | 11 .. 16 =>
         Start := 1 + Index (Image, "#");
         Stop  := Image'Last - 1;
      end case;

      return Adjust (Image (Start .. Stop), Width, False, Zero_Filled);
   end Modular_Image;

   function Float_Image (Value       : Number;
                         Fore        : Field       := 2;
                         Aft         : Field       := Number'Digits - 1;
                         Exp         : Field       := 3;
                         Zero_Filled : Boolean     := False;
                         Base        : Number_Base := 10)
   return String is
      package Number_IO is new Float_IO (Number);

      use Ada.Strings.Unbounded;
      use Conversions.Unbounded_Strings;

      function Base_10_Image return String is
         Image : String (1 .. 3 * Field'Last + 3);
         Start : Natural;
         Width : Field := Fore + Aft + 1;
      begin -- Base_10_Image
         Number_IO.Put (To => Image, Item => abs Value, Aft => Aft, Exp => Exp);
         Start := Index_Non_Blank (Image);

         if Exp > 0 then
            Width := Width + Exp + 1;
         end if;

         return Adjust (Image (Start .. Image'Last), Width, Value < 0.0, Zero_Filled);
      end Base_10_Image;

      function Non_10_Image return String is
         function Hex_Digit (Value : Natural) return Character is
            (if Value < 10 then Character'Val (Character'Pos ('0') + Value)
             else Character'Val (Character'Pos ('A') + Value - 10) );

         Work  : Number := abs Value;
         Image : String (1 .. 12 * Field'Last + 3);
         Start : Natural;
         Dot   : Natural;
         Col   : Number := 1.0 / Number (Base);
         Digit : Natural;

         use Unbounded_Numbers;
      begin -- Non_10_Image
         Number_IO.Put (To => Image, Item => Work, Aft => Aft, Exp => Exp);
         Start := Index_Non_Blank (Image);
         Dot := Index (Image, ".");

         Integer_Part : declare
            Full_Image : Unbounded_String :=
               +Integers.Image (Integers.Value (Image (Start .. Dot - 1) ), Base => Integers.Base_Number (Base) ) & '.';
         begin -- Integer_Part
            Work := Work - Number'Value (Image (Start .. Dot) & '0');

            Extract_Fraction : for K in 1 .. 1_000 Loop
               exit Extract_Fraction when Work <= 0.0;

               Digit := Integer (Number'Truncation (Work / Col) );
               Append (Source => Full_Image, New_Item => Hex_Digit (Digit) );
               Work := Work - Number (Digit) * Col;
               Col := Col / Number (Base);
            end loop Extract_Fraction;

            if Element (Full_Image, Length (Full_Image) ) = '.' then
               Append (Source => Full_Image, New_Item => '0');
            end if;

            return Adjust (+Full_Image, Fore + Aft + 1, Value < 0.0, Zero_Filled);
         end Integer_Part;
      end Non_10_Image;
   begin -- Float_Image
      if Base = 10 then
         return Base_10_Image;
      end if;

      return Non_10_Image;
   end Float_Image;
end PragmARC.Images;
