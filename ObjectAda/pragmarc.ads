-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Root package for PragmAda Reusable Components
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2002 May 01     J. Carter          V1.1--Added Too_Short
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package PragmARC with Pure is
   Empty : exception; -- Raised by components when an attempt is made to access data in an empty structure

   Full : exception; -- Raised by bounded components when an attempt is made to add data to a full structure

   Storage_Exhausted : exception;
   -- Raised by unbounded components when there is not enough memory available for an operation

   Too_Short : exception; -- Raised by bounded components if not enough room
end PragmARC;
