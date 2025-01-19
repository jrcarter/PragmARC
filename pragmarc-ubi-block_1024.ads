-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2025 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of UBI for blocks of 1024 bits (UBI-1024)
--
-- 2025 Feb 01     J. Carter     V1.0--Initial version

with PragmARC.Encryption.Threefish.Block_1024;

package PragmARC.UBI.Block_1024 is
   subtype Block is Encryption.Threefish.Block_1024.Block;

   function Compress (Message : in Byte_List; Init_Key : in Block; Init_Tweak : in Couple) return Block with
      Pre => Init_Tweak (0)                         = 0 and -- Position = 0
             Init_Tweak (1) rem 2 ** 48             = 0 and -- Position and reserved = 0
             (Init_Tweak (1) rem 2 ** 55) / 2 ** 54 = 0 and -- BitPad = 0
             Init_Tweak (1) / 2 ** 62               = 0;    -- First and Final = 0
   -- UBI compresses Message to a block using Init_Key and Init_Tweak as the initial values for Key and Tweak
   -- As recommended, Message is presumed to consist of whole bytes; BitPad will always be zero
   -- Given the definition of Byte_List, Position will never hold a value > Integer'Last < 2 ** 64
end PragmARC.UBI.Block_1024;
