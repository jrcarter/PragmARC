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
-------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.1--Use mode out to allow scalars
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Task_Communication.Monitors is
   protected body Monitor is
      procedure Put (Item : in Element) is
         -- Empty
      begin -- Put
         Value := Item;
      end Put;

      function Get return Element is
         (Value);
   end Monitor;
end PragmARC.Task_Communication.Monitors;
