-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2025 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- 2025 Feb 01     J. Carter     V1.0--Initial version

with PragmARC.UBI.Block_512;
with PragmARC.Encryption.Threefish.Block_512;

package body PragmARC.Skein.Block_512 is
   subtype Block is UBI.Block_512.Block;

   function Hash (Message : in Byte_List; Num_Bytes : in Positive) return Byte_List is
      function Output (Key : in Block) return Byte_List;
      -- Generates Num_Bytes output bytes from Key

      function Output (Key : in Block) return Byte_List is
         subtype Block_As_Bytes is Encryption.Threefish.Block_512.Block_As_Bytes;

         Tweak : constant Couple := (0 => 0, 1 => 63 * 2 ** 56); -- Type of Output

         Result : Byte_List (1 .. Num_Bytes);
         Last   : Natural := 0; -- Last used position in Result
         Bunch  : Block_As_Bytes;
      begin -- Output
         Fill : for I in Word loop
            Bunch := Encryption.Threefish.Block_512.Bytes_From_Block
                        (UBI.Block_512.Compress (Encryption.Threefish.Bytes_From_Word (I), Key, Tweak) );

            Copy : for J in Bunch'Range loop
               Last := Last + 1;
               Result (Last) := Bunch (J);

               exit Fill when Last >= Result'Last;
            end loop Copy;
         end loop Fill;

         return Result;
      end Output;

      Config : constant Byte_List := (16#53#, 16#48#, 16#41#, 16#33#, -- "SHA3"
                                      1, 0,                           -- Version # 1, 2 bytes, little endian
                                      0, 0) &                         -- Reserved
                                      Encryption.Threefish.Bytes_From_Word (Word (Num_Bytes) * 8) & -- # of output bits
                                      (17 .. 32 => 0); -- Not tree hashing

      Key   : Block  := (others => 0);
      Tweak : Couple := (0 => 0, 1 => 4 * 2 ** 56); -- Type set to Configuration
   begin -- Hash
      Key := UBI.Block_512.Compress (Config, Key, Tweak);
      Tweak (1) := 48 * 2 ** 56; -- Type set to Message
      Key := UBI.Block_512.Compress (Message, Key, Tweak);

      return Output (Key);
   end Hash;
end PragmARC.Skein.Block_512;
