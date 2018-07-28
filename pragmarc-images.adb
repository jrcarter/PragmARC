-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2018 Aug 01     J. Carter          V1.2--Cleanup compiler warnings
-- 2006 Mar 01     J. Carter          V1.1--Added Float_Image
-- 2004 Apr 01     J. Carter          V1.0--Initial version
--
with Ada.Strings.Fixed;
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

   function Float_Image
      (Value : Number; Fore : Field := 2; Aft : Field := Number'digits - 1; Exp : Field := 3; Zero_Filled : Boolean := False)
   return String is
      package Number_IO is new Float_IO (Number);

      Image : String (1 .. 3 * Field'Last + 3);
      Start : Natural;
      Width : Field := Fore + Aft + 1;
   begin -- Float_Image
      Number_IO.Put (To => Image, Item => abs Value, Aft => Aft, Exp => Exp);
      Start := Index_Non_Blank (Image);

      if Exp > 0 then
         Width := Width + Exp + 1;
      end if;

      return Adjust (Image (Start .. Image'Last), Width, Value < 0.0, Zero_Filled);
   end Float_Image;
end PragmARC.Images;
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
