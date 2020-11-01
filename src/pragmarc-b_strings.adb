-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Jun 01     J. Carter          V1.2--Use existing Too_Short exception rather than a local exception
-- 2016 Jul 01     J. Carter          V1.1--Made type B_String tagged and non-limited
-- 2015 Nov 15     J. Carter          V1.0--Initial release
--

package body PragmARC.B_Strings is
   function To_String (Source : B_String) return String is
      (Source.Value (1 .. Source.Len) );

   function To_B_String (Source : String) return B_String is
      Max_Length : constant Positive := Integer'Max (1, Source'Length);
      Length     : constant Natural  := Source'Length;
   begin -- To_B_String
      return (Max_Length => Max_Length, Len => Length, Value => Source & (Length + 1 .. Max_Length => ' ') );
   end To_B_String;

   function Length (Source : B_String) return Natural is
      (Source.Len);

   procedure Assign (To : in out B_String; From : in B_String) is
      -- Empty
   begin -- Assign
      To.Len := From.Len;
      To.Value (1 .. To.Len) := From.Value (1 .. To.Len);
   end Assign;

   procedure Assign (To : in out B_String; From : in String) is
      -- Empty
   begin -- Assign
      To.Len := From'Length;
      To.Value (1 .. To.Len) := From;
   end Assign;

   function "=" (Left : B_String; Right : B_String) return Boolean is
      (Left.Len = Right.Len and then Left.Value (1 .. Left.Len) = Right.Value (1 .. Right.Len) );

   function "<" (Left : B_String; Right : B_String) return Boolean is
      (Left.Value (1 .. Left.Len) < Right.Value (1 .. Right.Len) );

   function "<=" (Left : B_String; Right : B_String) return Boolean is
      (not (Left > Right) );

   function ">" (Left : B_String; Right : B_String) return Boolean is
     (Left.Value (1 .. Left.Len) > Right.Value (1 .. Right.Len) );

   function ">=" (Left : B_String; Right : B_String) return Boolean is
      (not (Left < Right) );
end PragmARC.B_Strings;
