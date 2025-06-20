-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2025 Jul 01     J. Carter          V2.1--Use SPDX license format
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Binary_Semaphore_Handler is
   protected body Binary_Semaphore is
      entry Request when not In_Use is
         -- Empty
      begin -- Request
         In_Use := True;
      end Request;

      entry Release when In_Use is
         -- Empty
      begin -- Release
         In_Use := False;
      end Release;
   end Binary_Semaphore;
end PragmARC.Binary_Semaphore_Handler;
