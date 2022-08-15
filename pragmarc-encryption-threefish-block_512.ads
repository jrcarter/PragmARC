-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of the Threefish cipher for blocks of 512 bits (Threefish-512)
--
-- 2022 Aug 15     J. Carter              V1.1--Add "with Preelaborate"
-- 2022 Feb 01     D. Norte de Moraes     V1.0--Initial 512-bit version derived from 256-bit version
--
package PragmARC.Encryption.Threefish.Block_512 with Preelaborate is
   Num_Words : constant := 8; -- A block is Num_Words words

   subtype Block  is Word_List (0 .. Num_Words - 1);

   type Key_Schedule_Handle is limited private; -- Initial value: not Valid

   function Valid (Key_Schedule : in Key_Schedule_Handle) return Boolean;
   -- Returns True if Key_Schedule has been created by Create_Key_Schedule; False otherwise

   procedure Create_Key_Schedule (Key : in Block; Tweak : in Couple; Key_Schedule : out Key_Schedule_Handle) with
      Post => Valid (Key_Schedule);
   -- Creates Key_Schedule from Key and Tweak

   Num_Rounds : constant := 72; -- For full encryption

   subtype Round_ID is Natural range 0 .. Num_Rounds - 1;

   procedure Encrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block; Last_Round : in Round_ID := Round_ID'Last) with
      Pre => Valid (Key_Schedule);
   -- Encrypts Text using Key_Schedule, which should have been created using Create_Key_Schedule,
   -- using rounds numbered 0 .. Last_Round
   -- The default should always be used for encryption
   -- For encryption-based random-number generators, a smaller value may be used (Threefry uses 20 rounds, 0 .. 19)

   procedure Decrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block) with
      Pre => Valid (Key_Schedule);
   -- Decrypts Text, which should have been encrypted with Encrypt and Key_Schedule
   -- Decryption always uses the full Num_Rounds rounds

   type Block_List is array (Positive range <>) of Block;

   function Encrypt (Key_Schedule : in Key_Schedule_Handle; Text : in Byte_List) return Block_List with
      Pre => Valid (Key_Schedule);
   -- Pads Text to a multiple of 64 bytes with zeros, then converts 64-byte slices using Block_From_Bytes and Key_Schedule

   function Decrypt (Key_Schedule : in Key_Schedule_Handle; Text : in Block_List) return Byte_List with
      Pre => Valid (Key_Schedule);
   -- Decrypts the blocks of Text and converts the results to a Byte_List using Bytes_From_Block
   -- Results includes any padding added by Encrypt

   subtype Block_As_Bytes is Byte_List (1 .. 64); -- 8 consecutive Word_As_Bytes

   function Block_From_Bytes (List : in Block_As_Bytes) return Block;
   function Bytes_From_Block (Value : in Block) return Block_As_Bytes;
   -- Endian-independent conversion using Word_From_Bytes and Bytes_From_Word
private -- PragmARC.Encryption.Threefish
   type Key_List is array (0 .. Num_Rounds / 4) of Block;

   type Key_Schedule_Handle is record
      Key    : Word_List (0 .. Num_Words);
      Tweak  : Word_List (0 .. 2);
      Subkey : Key_List;
      Valid  : Boolean := False;
   end record;

   function Valid (Key_Schedule : in Key_Schedule_Handle) return Boolean is
      (Key_Schedule.Valid);
end PragmARC.Encryption.Threefish.Block_512;
