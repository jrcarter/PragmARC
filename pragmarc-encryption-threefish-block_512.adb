-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- 2022 Feb 01     D. Norte de Moraes     V1.0--Initial 512-bit version derived from 256-bit version
--
package body PragmARC.Encryption.Threefish.Block_512 is
   use type Word;

   procedure Create_Key_Schedule (Key : in Block; Tweak : in Couple; Key_Schedule : out Key_Schedule_Handle) is
      C240    : constant := 16#1BD1_1BDA_A9FC_1A22#;
      Modulus : constant := Num_Words + 1;

      Ks : Key_Schedule_Handle renames Key_Schedule;
   begin -- Create_Key_Schedule
      Ks.Key (Key'Range) := Key;
      Ks.Tweak (Tweak'Range) := Tweak;
      Ks.Key (Ks.Key'Last) := C240;

      Xor_All : for I in Key'Range loop
         Ks.Key (Ks.Key'Last) := Ks.Key (Ks.Key'Last) xor Key (I);
      end loop Xor_All;

      Ks.Tweak (Ks.Tweak'Last) := Tweak (Tweak'First) xor Tweak (Tweak'Last);

      All_Subkeys : for S in Ks.Subkey'Range loop
         Ks.Subkey (S) (0) := Ks.Key (S rem Modulus);
         Ks.Subkey (S) (1) := Ks.Key ( (S + 1) rem Modulus);
         Ks.Subkey (S) (2) := Ks.Key ( (S + 2) rem Modulus);
         Ks.Subkey (S) (3) := Ks.Key ( (S + 3) rem Modulus);
         Ks.Subkey (S) (4) := Ks.Key ( (S + 4) rem Modulus);
         Ks.Subkey (S) (5) := Ks.Key ( (S + 5) rem Modulus) + Ks.Tweak (S rem 3);
         Ks.Subkey (S) (6) := Ks.Key ( (S + 6) rem Modulus) + Ks.Tweak ( (S + 1) rem 3);
         Ks.Subkey (S) (7) := Ks.Key ( (S + 7) rem Modulus) + Word (S);
      end loop All_Subkeys;

      Ks.Valid := True;
   end Create_Key_Schedule;

   type Four_Id is (First, Second, Third, Last); -- 4 pairs per Block

   type Rotation_Amount is array (0 .. 7, Four_Id) of Positive; -- Indexed by Round mod 8

   Rot : constant Rotation_Amount := (0 => (First => 46, Second => 36, Third => 19, Last => 37),
                                      1 => (First => 33, Second => 27, Third => 14, Last => 42),
                                      2 => (First => 17, Second => 49, Third => 36, Last => 39),
                                      3 => (First => 44, Second =>  9, Third => 54, Last => 56),
                                      4 => (First => 39, Second => 30, Third => 34, Last => 24),
                                      5 => (First => 13, Second => 50, Third => 10, Last => 17),
                                      6 => (First => 25, Second => 29, Third => 39, Last => 43),
                                      7 => (First =>  8, Second => 35, Third => 56, Last => 22) );

   procedure Encrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block; Last_Round : in Round_ID := Round_ID'Last) is
      function "+" (Left : Block; Right : Block) return Block; -- Word-by-word addition without carry

      procedure Mix (Round : in Round_Id; Side : in Four_Id; Pair : in out Couple); -- Performs the MIX operation in place

      procedure Permute (Text : in out Block);

      function "+" (Left : Block; Right : Block) return Block is
         (Left (0) + Right (0), Left (1) + Right (1), Left (2) + Right (2), Left (3) + Right (3),
          Left (4) + Right (4), Left (5) + Right (5), Left (6) + Right (6), Left (7) + Right (7) );

      procedure Mix (Round : in Round_Id; Side : in Four_Id; Pair : in out Couple) is
         -- Empty
      begin -- Mix
         Pair (Pair'First) := Pair (Pair'First) + Pair (Pair'Last);
         Pair (Pair'Last)  := Interfaces.Rotate_Left (Pair (Pair'Last), Rot (Round rem 8, Side) ) xor Pair (Pair'First);
      end Mix;

      procedure Permute (Text : in out Block) is
         Temp0 : constant Word := Text (0);
         Temp3 : constant Word := Text (3);
      begin -- Permute
         Text (0) := Text (2);
         Text (2) := Text (4);
         Text (3) := Text (7);
         Text (4) := Text (6);
         Text (6) := Temp0;
         Text (7) := Temp3;
      end Permute;

      Ks : Key_Schedule_Handle renames Key_Schedule;
   begin -- Encrypt
      All_Rounds : for Round in 0 .. Last_Round loop
         if Round rem 4 = 0 then
            Text := Text + Ks.Subkey (Round / 4);
         end if;

         Mix (Round => Round, Side => First,  Pair => Text (0 .. 1) );
         Mix (Round => Round, Side => Second, Pair => Text (2 .. 3) );
         Mix (Round => Round, Side => Third,  Pair => Text (4 .. 5) );
         Mix (Round => Round, Side => Last,   Pair => Text (6 .. 7) );
         Permute (Text => Text);
      end loop All_Rounds;

      Text := Text + Ks.Subkey ( (Last_Round + 1) / 4);
   end Encrypt;

   procedure Decrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block) is
      function "-" (Left : Block; Right : Block) return Block; -- Word-by-word subtraction without borrow

      procedure Unmix (Round : in Round_Id; Side : in Four_Id; Pair : in out Couple); -- Inverse of the Mix procedure

      procedure Permute (Text : in out Block); -- Inverse of the encryption Permute procedure

      function "-" (Left : Block; Right : Block) return Block is
         (Left (0) - Right (0), Left (1) - Right (1), Left (2) - Right (2), Left (3) - Right (3),
          Left (4) - Right (4), Left (5) - Right (5), Left (6) - Right (6), Left (7) - Right (7) );

      procedure Unmix (Round : in Round_Id; Side : in Four_Id; Pair : in out Couple) is
         -- Empty
      begin -- Unmix
         Pair (Pair'Last)  := Interfaces.Rotate_Right (Pair (Pair'Last) xor Pair (Pair'First), Rot (Round rem 8, Side) );
         Pair (Pair'First) := Pair (Pair'First) - Pair (Pair'Last);
      end Unmix;

      procedure Permute (Text : in out Block) is
         Temp2 : constant Word := Text (2);
         Temp4 : constant Word := Text (4);
         Temp6 : constant Word := Text (6);
         Temp7 : constant Word := Text (7);
      begin -- Permute
         Text (2) := Text (0);
         Text (4) := Temp2;
         Text (7) := Text (3);
         Text (6) := Temp4;
         Text (0) := Temp6;
         Text (3) := Temp7;
      end Permute;

      Ks : Key_Schedule_Handle renames Key_Schedule;
   begin -- Decrypt
      Text := Text - Ks.Subkey (Num_Rounds / 4);

      All_Rounds : for Round in reverse Round_Id loop
         Permute (Text => Text);
         Unmix (Round => Round, Side => Last,   Pair => Text (6 .. 7) );
         Unmix (Round => Round, Side => Third,  Pair => Text (4 .. 5) );
         Unmix (Round => Round, Side => Second, Pair => Text (2 .. 3) );
         Unmix (Round => Round, Side => First,  Pair => Text (0 .. 1) );

         if Round rem 4 = 0 then
            Text := Text - Ks.Subkey (Round / 4);
         end if;
      end loop All_Rounds;
   end Decrypt;

   Bytes_Per_Block : constant := 64;

   function Encrypt (Key_Schedule : in Key_Schedule_Handle; Text : in Byte_List) return Block_List is
      Num_Blocks : constant Natural := (Text'Length + Bytes_Per_Block - 1) / Bytes_Per_Block;

      Slice  : Block;
      Result : Block_List (1 .. Num_Blocks);
      Start  : Positive := Text'First;
   begin -- Encrypt
      All_Blocks : for I in Result'Range loop
         if Start + Bytes_Per_Block - 1 > Text'Last then
            Slice := Block_From_Bytes (Text (Start .. Text'Last) & (Text'Last + 1 .. Start + Bytes_Per_Block - 1 => 0) );
         else
            Slice := Block_From_Bytes (Text (Start .. Start + Bytes_Per_Block - 1) );
         end if;

         Start := Start + Bytes_Per_Block;
         Encrypt (Key_Schedule => Key_Schedule, Text => Slice);
         Result (I) := Slice;
      end loop All_Blocks;

      return Result;
   end Encrypt;

   function Decrypt (Key_Schedule : in Key_Schedule_Handle; Text : in Block_List) return Byte_List is
      Num_Bytes : constant Natural := Text'Length * Bytes_Per_Block;

      Slice  : Block;
      Result : Byte_List (1 .. Num_Bytes);
      Start  : Positive := 1;
   begin -- Decrypt
      All_Blocks : for I in Text'Range loop
         Slice := Text (I);
         Decrypt (Key_Schedule => Key_Schedule, Text => Slice);
         Result (Start .. Start + Bytes_Per_Block - 1) := Bytes_From_Block (Slice);
         Start := Start + Bytes_Per_Block;
      end loop All_Blocks;

      return Result;
   end Decrypt;

   function Block_From_Bytes (List : in Block_As_Bytes) return Block is
      Result : Block;
      Start  : Positive := List'First;
   begin -- Block_From_Bytes
      Convert : for I in Result'Range loop
         Result (I) := Word_From_Bytes (List (Start .. Start + Word_As_Bytes'Length - 1) );
         Start := Start + Word_As_Bytes'Length;
      end loop Convert;

      return Result;
   end Block_From_Bytes;

   function Bytes_From_Block (Value : in Block) return Block_As_Bytes is
      Result : Block_As_Bytes;
      Start  : Positive := Result'First;
   begin -- Bytes_From_Block
      Convert : for I in Value'Range loop
         Result (Start .. Start + Word_As_Bytes'Length - 1) := Bytes_From_Word (Value (I) );
         Start := Start + Word_As_Bytes'Length;
      end loop Convert;

      return Result;
   end Bytes_From_Block;
end PragmARC.Encryption.Threefish.Block_512;
