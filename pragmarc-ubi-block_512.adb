-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2025 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- 2025 Feb 01     J. Carter     V1.0--Initial version

package body PragmARC.UBI.Block_512 is
   function Compress (Message : in Byte_List; Init_Key : in Block; Init_Tweak : in Couple) return Block is
      subtype Block_List is Encryption.Threefish.Block_512.Block_List;
      use type Block_List;

      Bytes_Per_Block : constant Positive := Encryption.Threefish.Block_512.Block_As_Bytes'Length;

      function To_Block_List (Message : in Byte_List) return Block_List is
         (if Message'Length = 0 then (1 .. 0 => <>)
          else Encryption.Threefish.Block_512.Block_From_Bytes (Message (Message'First .. Message'First + Bytes_Per_Block - 1) ) &
               To_Block_List (Message (Message'First + Bytes_Per_Block .. Message'Last) ) );
      -- Converts Message, with Message'Length a multiple of Bytes_Per_Block, to a Block_List

      function "xor" (Left : in Block; Right : in Block) return Block is
         (Left (0) xor Right (0), Left (1) xor Right (1), Left (2) xor Right (2), Left (3) xor Right (3),
          Left (4) xor Right (4), Left (5) xor Right (5), Left (6) xor Right (6), Left (7) xor Right (7) );
      -- xor for a Block, done Word by Word

      Remainder : constant Natural    := Message'Length rem Bytes_Per_Block;
      Padded    : constant Byte_List  := Message & (1 .. (if Remainder = 0 then 0 else Bytes_Per_Block - Remainder) => 0);
      M         : constant Block_List := To_Block_List (Padded);

      Result : Block  := Init_Key;
      Tweak  : Couple := Init_Tweak;
      Key    : Block;
      KS     : Encryption.Threefish.Block_512.Key_Schedule_Handle;
   begin -- Compress
      All_Blocks : for K in M'Range loop
         Key := Result;
         Tweak (0) := (if K = M'Last then Message'Length else Tweak (0) + Word (Bytes_Per_Block) );
         Tweak (1) := Init_Tweak (1) + (if K = M'First then 2 ** 62 else 0) + -- First set
                                       (if K = M'Last  then 2 ** 63 else 0);  -- Final set
         Encryption.Threefish.Block_512.Create_Key_Schedule (Key => Key, Tweak => Tweak, Key_Schedule => KS);
         Result := M (K);
         Encryption.Threefish.Block_512.Encrypt (Key_Schedule => KS, Text => Result);
         Result := Result xor M (K);
      end loop All_Blocks;

      return Result;
   end Compress;
end PragmARC.UBI.Block_512;
