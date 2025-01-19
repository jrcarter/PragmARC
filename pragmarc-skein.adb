-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2025 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- 2025 Feb 01     J. Carter     V1.0--Initial version

with PragmARC.Skein.Block_256;
with PragmARC.Skein.Block_512;
with PragmARC.Skein.Block_1024;

package body PragmARC.Skein is
   function Hash (Message : in Byte_List; Num_Bytes : in Positive; Block_Size : in Block_Size_ID) return Byte_List is
      (case Block_Size is
       when Size_256  => Block_256.Hash  (Message, Num_Bytes),
       when Size_512  => Block_512.Hash  (Message, Num_Bytes),
       when Size_1024 => Block_1024.Hash (Message, Num_Bytes) );
end PragmARC.Skein;
