-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Bounded, variable-length strings that are hopefully more usable than
-- Ada.Strings.Bounded provides
-- Based on an idea by Robert Duff presented on comp.lang.ada
--
-- History:
-- 2016 Mar 15     J. Carter          V1.2--Default discriminant doesn't work as Duff claimed, at least with GNAT
-- 2016 Feb 15     J. carter          V1.1--Forgot "+" for To_B_String
-- 2015 Nov 15     J. Carter          V1.0--Initial release
--
with Ada.Strings;

package PragmARC.B_Strings is
   type B_String (Max_Length : Positive) is limited private;
   -- Default initial value is Null_B_String

   Null_B_String : constant B_String; -- A string of zero characters

   function To_String (Source : B_String) return String;
   function "+" (Source : B_String) return String renames To_String;
   -- Lower bound of result is 1; upper bound is Length (Source)

   function To_B_String (Source : String) return B_String;
   function "+" (Source : String) return B_String renames To_B_String;
   -- Result's Max_Length will be Source'Length, or 1 if Source'Lenth = 0

   function Length (Source : B_String) return Natural;

   Too_Long : exception;

   procedure Assign (To : in out B_String; From : in B_String; Drop : in Ada.Strings.Truncation);
   -- Gives To the same value as From
   -- If Drop = Error and Length (From) > To.Max_Length, raises Too_Long
   -- To is unchanged if Too_Long is raised

   procedure Assign (To : in out B_String; From : in String; Drop : in Ada.Strings.Truncation);
   -- Same as Assign (To => To, From => +From, Drop => Drop);

   function "="  (Left : B_String; Right : B_String) return Boolean;
   function "<"  (Left : B_String; Right : B_String) return Boolean;
   function "<=" (Left : B_String; Right : B_String) return Boolean;
   function ">"  (Left : B_String; Right : B_String) return Boolean;
   function ">=" (Left : B_String; Right : B_String) return Boolean;
private -- PragmARC.B_Strings
   type B_String (Max_Length : Positive) is limited record
      Length : Natural := 0;
      Value  : String (1 .. Max_Length) := (1 .. Max_Length => ' ');
   end record;

   Null_B_String : constant B_String := (Max_Length => 1, others => <>);
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
