-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Bounded, variable-length strings that are hopefully more usable than
-- Ada.Strings.Bounded provides
-- Modified from an idea by Robert Duff presented on comp.lang.ada
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2020 Jun 01     J. Carter          V1.4--Use existing Too_Short exception rather than a local exception
-- 2016 Jul 01     J. Carter          V1.3--Made type B_String tagged and non-limited
-- 2016 Mar 15     J. Carter          V1.2--Default discriminant doesn't work as Duff claimed, at least with GNAT
-- 2016 Feb 15     J. carter          V1.1--Forgot "+" for To_B_String
-- 2015 Nov 15     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.B_Strings is
   type B_String (Max_Length : Positive := 1024) is tagged limited private;
   -- Default initial value is Null_B_String

   Null_B_String : constant B_String; -- A string of zero characters

   function To_String (Source : B_String) return String with
      Post => To_String'Result'First = 1 and To_String'Result'Last = Source.Length;
   function "+" (Source : B_String) return String renames To_String;

   function To_B_String (Source : String) return B_String with
      Post => To_B_String'Result.Max_Length = Integer'Max (Source'Length, 1);
   function "+" (Source : String) return B_String renames To_B_String;

   function Length (Source : B_String) return Natural;

   procedure Assign (To : in out B_String; From : in B_String) with
      Pre  => From.Length <= To.Max_Length or else raise Too_Short,
      Post => To = From;

   procedure Assign (To : in out B_String; From : in String) with
      Pre  => From'Length <= To.Max_Length or else raise Too_Short,
      Post => +To = From;
   -- Same as Assign (To => To, From => +From);

   function "="  (Left : B_String; Right : B_String) return Boolean;
   function "<"  (Left : B_String; Right : B_String) return Boolean;
   function "<=" (Left : B_String; Right : B_String) return Boolean;
   function ">"  (Left : B_String; Right : B_String) return Boolean;
   function ">=" (Left : B_String; Right : B_String) return Boolean;
private -- PragmARC.B_Strings
   type B_String (Max_Length : Positive := 1024) is tagged limited record
      Len   : Natural := 0;
      Value : String (1 .. Max_Length) := (1 .. Max_Length => ' ');
   end record;

   Null_B_String : constant B_String := (Max_Length => 1, others => <>);
end PragmARC.B_Strings;
