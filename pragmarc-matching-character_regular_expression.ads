-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Instantiation of PragmARC.Regular_Expression_Matcher for strings
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2019 Apr 15     J. Carter          V1.2--Provide ranges in classes
-- 2016 Jun 01     J. Carter          V1.1--Revised formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with PragmARC.Matching.Regular_Expression;

package PragmARC.Matching.Character_Regular_Expression with Preelaborate is
   Any_Item         : constant Character := '?';
   Escape_Item      : constant Character := '&';
   Not_Item         : constant Character := '~';
   Closure_Item     : constant Character := '*';
   Start_Class_Item : constant Character := '[';
   Stop_Class_Item  : constant Character := ']';
   Begin_Set_Item   : constant Character := '#';
   End_Set_Item     : constant Character := '$';

   package Regexp is new PragmARC.Matching.Regular_Expression (Item             => Character,
                                                               Index            => Positive,
                                                               Item_Set         => String,
                                                               Any_Item         => Any_Item,
                                                               Escape_Item      => Escape_Item,
                                                               Not_Item         => Not_Item,
                                                               Closure_Item     => Closure_Item,
                                                               Start_Class_Item => Start_Class_Item,
                                                               Stop_Class_Item  => Stop_Class_Item,
                                                               Begin_Set_Item   => Begin_Set_Item,
                                                               End_Set_Item     => End_Set_Item,
                                                               "=" => "=");

end PragmARC.Matching.Character_Regular_Expression;
