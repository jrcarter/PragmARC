-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Min_Max is
   function Min (Left : Element; Right : Element) return Element is
      (if Left < Right then Left else Right);

   function Max (Left : Element; Right : Element) return Element is
      (if Left < Right then Right else Left);
end PragmARC.Min_Max;
