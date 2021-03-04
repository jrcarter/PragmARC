-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Wrapping is
   function Wrap_Pred (Value : Item) return Item is
      (if Value = Item'First then Item'Last else Item'Pred (Value) );

   function Wrap_Succ (Value : Item) return Item is
      (if Value = Item'Last then Item'First else Item'Succ (Value) );
end PragmARC.Wrapping;
