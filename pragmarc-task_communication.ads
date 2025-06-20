-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Root of the Task_Communication hierarchy
-- Except for Monitors, these are mostly used with tasks that communicate by rendezvous
-- Protected queues are also used for task communication, but are found in the Data_Structures hierarchy
--
-- History:
-- 2025 Jul 01     J. Carter          V1.2--Use SPDX license format
-- 2020 Dec 01     J. Carter          V1.1--Changed elaboration pragmas to aspects
-- 2020 Nov 01     J. Carter          V1.0--Initial Ada-12 version
--
package PragmARC.Task_Communication with Pure is
   -- Empty
end PragmARC.Task_Communication;
