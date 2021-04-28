-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Provides equivalents to the 'Pred and 'Succ functions that wrap around from 'First to 'Last and from 'Last to 'First
--
-- History:
-- 2021 May 01     J. Carter          V2.2--Adhere to coding standard
-- 2020 Dec 01     J. Carter          V2.1--Expression functions eliminate body; changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Wrapping
   type Item is (<>);
package PragmARC.Wrapping with Pure is
   function Wrap_Pred (Value : in Item) return Item is
      (if Value = Item'First then Item'Last else Item'Pred (Value) );

   function Wrap_Succ (Value : in Item) return Item is
      (if Value = Item'Last then Item'First else Item'Succ (Value) );
end PragmARC.Wrapping;
