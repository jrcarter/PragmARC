-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Fast string search routine; faster than Boyer-Moore for most reasonable search patterns
-- From Sunday, David, "A very fast substring search algorithm", COMMUNICATIONS OF THE ACM, 1990 Aug
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Matching.Quick_String with Pure is
   type Result (Found : Boolean := False) is record
      case Found is
      when False =>
         null;
      when True =>
         Index : Positive := 1;
      end case;
   end record;

   function Location (Pattern : String; Source : String) return Result;
   -- If Pattern occurs in Source, returns (Found => True, Index => index of first position of Pattern in Source)
   -- Returns (Found => False) otherwise
end PragmARC.Matching.Quick_String;
