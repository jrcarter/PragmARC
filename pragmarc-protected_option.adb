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
