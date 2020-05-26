-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2020 Jun 01     J. Carter          V1.2--Use existing Too_Short exception rather than a local exception
-- 2016 Jul 01     J. Carter          V1.1--Made type B_String tagged and non-limited
-- 2015 Nov 15     J. Carter          V1.0--Initial release
--

package body PragmARC.B_Strings is
   function To_String (Source : B_String) return String is
      -- Empty declarative part
   begin -- To_String
      return Source.Value (1 .. Source.Length);
   end To_String;

   function To_B_String (Source : String) return B_String is
      Max_Length : constant Positive := Integer'Max (1, Source'Length);
      Length     : constant Natural  := Source'Length;
   begin -- To_B_String
      return (Max_Length => Max_Length, Len => Length, Value => Source & (Length + 1 .. Max_Length => ' ') );
   end To_B_String;

   function Length (Source : B_String) return Natural is
      -- Empty declarative part
   begin -- Length
      return Source.Len;
   end Length;

   procedure Assign (To : in out B_String; From : in B_String; Drop : in Ada.Strings.Truncation := Ada.Strings.Error) is
      use type Ada.Strings.Truncation;
   begin -- Assign
      if From.Len <= To.Max_Length then
         To.Len := From.Len;
         To.Value (1 .. To.Len) := From.Value (1 .. To.Len);

         return;
      end if;

      -- From.Len > To.Max_Length

      if Drop = Ada.Strings.Error then
         raise Too_Short;
      end if;

      To.Len := To.Max_Length;

      if Drop = Ada.Strings.Left then
         To.Value := From.Value (From.Len - To.Len + 1 .. From.Len);

         return;
      end if;

      -- Drop = Right

      To.Value := From.Value (1 .. To.Len);
   end Assign;

   procedure Assign (To : in out B_String; From : in String; Drop : in Ada.Strings.Truncation := Ada.Strings.Error) is
      use type Ada.Strings.Truncation;
   begin -- Assign
      if From'Length <= To.Max_Length then
         To.Len := From'Length;
         To.Value (1 .. To.Len) := From;

         return;
      end if;

      -- From'Length > To.Max_Length

      if Drop = Ada.Strings.Error then
         raise Too_Short;
      end if;

      To.Len := To.Max_Length;

      if Drop = Ada.Strings.Left then
         To.Value := From (From'Last - To.Len + 1 .. From'Last);

         return;
      end if;

      -- Drop = Right

      To.Value := From (From'First .. From'First + To.Len - 1);
   end Assign;

   function "=" (Left : B_String; Right : B_String) return Boolean is
      -- Empty declarative part
   begin -- "="
      return Left.Len = Right.Len and then Left.Value (1 .. Left.Len) = Right.Value (1 .. Right.Len);
   end "=";

   function "<" (Left : B_String; Right : B_String) return Boolean is
      -- Empty declarative part
   begin -- "<"
      return Left.Value (1 .. Left.Len) < Right.Value (1 .. Right.Len);
   end "<";

   function "<=" (Left : B_String; Right : B_String) return Boolean is
      -- Empty declarative part
   begin -- "<="
      return not (Left > Right);
   end "<=";

   function ">" (Left : B_String; Right : B_String) return Boolean is
      -- Empty declarative part
   begin -- ">"
      return Left.Value (1 .. Left.Len) > Right.Value (1 .. Right.Len);
   end ">";

   function ">=" (Left : B_String; Right : B_String) return Boolean is
      -- Empty declarative part
   begin -- ">="
      return not (Left < Right);
   end ">=";
end PragmARC.B_Strings;
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
