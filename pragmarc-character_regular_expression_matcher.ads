-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Instantiation of PragmARC.Regular_Expression_Matcher for strings
--
-- History:
-- 2019 Apr 15     J. Carter          V1.2--Provide ranges in classes
-- 2016 Jun 01     J. Carter          V1.1--Revised formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with PragmARC.Regular_Expression_Matcher;
package PragmARC.Character_Regular_Expression_Matcher is
   Any_Item         : constant Character := '?';
   Escape_Item      : constant Character := '&';
   Not_Item         : constant Character := '~';
   Closure_Item     : constant Character := '*';
   Start_Class_Item : constant Character := '[';
   Stop_Class_Item  : constant Character := ']';
   Begin_Set_Item   : constant Character := '#';
   End_Set_Item     : constant Character := '$';

   package Regexp is new PragmARC.Regular_Expression_Matcher (Item             => Character,
                                                              Index            => Positive,
                                                              Item_Set         => String,
                                                              Any_Item         => Any_Item,
                                                              Escape_Item      => Escape_Item,
                                                              Not_Item         => Not_Item,
                                                              Closure_Item     => Closure_Item,
                                                              Start_Class_Item => Start_Class_Item,
                                                              Stop_Class_Item  => Stop_Class_Item,
                                                              Begin_Set_Item   => Begin_Set_Item,
                                                              End_Set_Item     => End_Set_Item);

   function Expanded_Ranges (Pattern : String) return String;
   -- If Pattern contains classes with an unescaped hyphen ('-') that is not the first or last member of the class,
   -- replaces the 3 characters starting with the character before the hyphen and ending with the character after with the sequence
   -- of characters from the preceding character to the following
   -- For example, replaces "[0-9]" with "[0123456789]"
   -- If the first character follows the second in Character, the replacement is the null string

   Illegal_Pattern : exception renames Regexp.Illegal_Pattern;

   subtype Processed_Pattern is Regexp.Processed_Pattern;

   procedure Process (Pattern : in String; Processed : in out Processed_Pattern);
   -- Calls Regexp.Process (Pattern => Expanded_Ranges (Pattern), Processed => Processed);

   subtype Result is Regexp.Result;

   function Location (Pattern : Processed_Pattern; Source : String) return Result renames Regexp.Location;

   function Location (Pattern : String; Source : String) return Result;
   -- Returns Regexp.Location (Expanded_Ranges (Pattern), Source);
end PragmARC.Character_Regular_Expression_Matcher;
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
