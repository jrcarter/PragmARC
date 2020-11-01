-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Added comment for empty declarative part
-- 2001 Dec 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Protected_Option is
   protected body Handle is
      function Enabled return Boolean is
         (State);

      procedure Enable (Enabled : in Boolean := True) is
         -- Empty
      begin -- Enabled
         State := Enabled;
      end Enable;
   end Handle;
end PragmARC.Protected_Option;
