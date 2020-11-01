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
