-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Root package for PragmAda Reusable Components
--
-- History:
-- 2025 Jul 01     J. Carter          V2.2--Use SPDX license format
-- 2022 May 15     J. Carter          V2.1--Activated checks
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2002 May 01     J. Carter          V1.1--Added Too_Short
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC with Pure is
   Empty : exception; -- Raised by components when an attempt is made to access data in an empty structure

   Full : exception; -- Raised by bounded components when an attempt is made to add data to a full structure

   Storage_Exhausted : exception;
   -- Raised by unbounded components when there is not enough memory available for an operation

   Too_Short : exception; -- Raised by bounded components if not enough room
end PragmARC;
