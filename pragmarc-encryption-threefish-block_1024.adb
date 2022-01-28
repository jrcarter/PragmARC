-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- 2022 Feb 01     D. Norte de Moraes     V1.0--Initial 1024-bit version derived from 256-bit version
--
package body PragmARC.Encryption.Threefish.Block_1024 is
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
         Ks.Subkey (S) ( 0) := Ks.Key (S rem Modulus);
         Ks.Subkey (S) ( 1) := Ks.Key ( (S +  1) rem Modulus);
         Ks.Subkey (S) ( 2) := Ks.Key ( (S +  2) rem Modulus);
         Ks.Subkey (S) ( 3) := Ks.Key ( (S +  3) rem Modulus);
         Ks.Subkey (S) ( 4) := Ks.Key ( (S +  4) rem Modulus);
         Ks.Subkey (S) ( 5) := Ks.Key ( (S +  5) rem Modulus);
         Ks.Subkey (S) ( 6) := Ks.Key ( (S +  6) rem Modulus);
         Ks.Subkey (S) ( 7) := Ks.Key ( (S +  7) rem Modulus);
         Ks.Subkey (S) ( 8) := Ks.Key ( (S +  8) rem Modulus);
         Ks.Subkey (S) ( 9) := Ks.Key ( (S +  9) rem Modulus);
         Ks.Subkey (S) (10) := Ks.Key ( (S + 10) rem Modulus);
         Ks.Subkey (S) (11) := Ks.Key ( (S + 11) rem Modulus);
         Ks.Subkey (S) (12) := Ks.Key ( (S + 12) rem Modulus);

         Ks.Subkey (S) (13) := Ks.Key ( (S + 13) rem Modulus) + Ks.Tweak (S rem 3);
         Ks.Subkey (S) (14) := Ks.Key ( (S + 14) rem Modulus) + Ks.Tweak ( (S + 1) rem 3);
         Ks.Subkey (S) (15) := Ks.Key ( (S + 15) rem Modulus) + Word (S);
      end loop All_Subkeys;

      Ks.Valid := True;
   end Create_Key_Schedule;

   type Eight_Id is (First, Second, Third, Fourth, Fifth, Sixth, Seventh, Last); -- 8 pairs per Block

   type Rotation_Amount is array (0 .. 7, Eight_Id) of Positive; -- Indexed by Round mod 8

   Rot : constant Rotation_Amount :=
      (0 => (First => 24, Second => 13, Third =>  8, Fourth => 47, Fifth =>  8, Sixth => 17, Seventh => 22, Last => 37),
       1 => (First => 38, Second => 19, Third => 10, Fourth => 55, Fifth => 49, Sixth => 18, Seventh => 23, Last => 52),
       2 => (First => 33, Second =>  4, Third => 51, Fourth => 13, Fifth => 34, Sixth => 41, Seventh => 59, Last => 17),
       3 => (First =>  5, Second => 20, Third => 48, Fourth => 41, Fifth => 47, Sixth => 28, Seventh => 16, Last => 25),
       4 => (First => 41, Second =>  9, Third => 37, Fourth => 31, Fifth => 12, Sixth => 47, Seventh => 44, Last => 30),
       5 => (First => 16, Second => 34, Third => 56, Fourth => 51, Fifth =>  4, Sixth => 53, Seventh => 42, Last => 41),
       6 => (First => 31, Second => 44, Third => 47, Fourth => 46, Fifth => 19, Sixth => 42, Seventh => 44, Last => 25),
       7 => (First =>  9, Second => 48, Third => 35, Fourth => 52, Fifth => 23, Sixth => 31, Seventh => 37, Last => 20) );

   procedure Encrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block; Last_Round : in Round_ID := Round_ID'Last) is
      function "+" (Left : Block; Right : Block) return Block; -- Word-by-word addition without carry

      procedure Mix (Round : in Round_Id; Side : in Eight_Id; Pair : in out Couple); -- Performs the MIX operation in place

      procedure Permute (Text : in out Block);

      function "+" (Left : Block; Right : Block) return Block is
         (Left ( 0) + Right ( 0), Left ( 1) + Right ( 1), Left ( 2) + Right ( 2), Left ( 3) + Right ( 3),
          Left ( 4) + Right ( 4), Left ( 5) + Right ( 5), Left ( 6) + Right ( 6), Left ( 7) + Right ( 7),
          Left ( 8) + Right ( 8), Left ( 9) + Right ( 9), Left (10) + Right (10), Left (11) + Right (11),
          Left (12) + Right (12), Left (13) + Right (13), Left (14) + Right (14), Left (15) + Right (15) );

      procedure Mix (Round : in Round_Id; Side : in Eight_Id; Pair : in out Couple) is
         -- Empty
      begin -- Mix
         Pair (Pair'First) := Pair (Pair'First) + Pair (Pair'Last);
         Pair (Pair'Last)  := Interfaces.Rotate_Left (Pair (Pair'Last), Rot (Round rem 8, Side) ) xor Pair (Pair'First);
      end Mix;

      procedure Permute (Text : in out Block) is
         Temp1 : constant Word := Text (1);
         Temp3 : constant Word := Text (3);
         Temp4 : constant Word := Text (4);
         Temp5 : constant Word := Text (5);
         Temp7 : constant Word := Text (7);
         Temp8 : constant Word := Text (8);
      begin -- Permute
         Text ( 1) := Text ( 9);
         Text ( 3) := Text (13);
         Text ( 4) := Text ( 6);
         Text ( 5) := Text (11);
         Text ( 6) := Temp4;
         Text ( 7) := Text (15);
         Text ( 8) := Text (10);
         Text ( 9) := Temp7;
         Text (10) := Text (12);
         Text (11) := Temp3;
         Text (12) := Text (14);
         Text (13) := Temp5;
         Text (14) := Temp8;
         Text (15) := Temp1;
      end Permute;

      Ks : Key_Schedule_Handle renames Key_Schedule;
   begin -- Encrypt
      All_Rounds : for Round in 0 .. Last_Round loop
         if Round rem 4 = 0 then
            Text := Text + Ks.Subkey (Round / 4);
         end if;

         Mix (Round => Round, Side => First,   Pair => Text ( 0 ..  1) );
         Mix (Round => Round, Side => Second,  Pair => Text ( 2 ..  3) );
         Mix (Round => Round, Side => Third,   Pair => Text ( 4 ..  5) );
         Mix (Round => Round, Side => Fourth,  Pair => Text ( 6 ..  7) );
         Mix (Round => Round, Side => Fifth,   Pair => Text ( 8 ..  9) );
         Mix (Round => Round, Side => Sixth,   Pair => Text (10 .. 11) );
         Mix (Round => Round, Side => Seventh, Pair => Text (12 .. 13) );
         Mix (Round => Round, Side => Last,    Pair => Text (14 .. 15) );
         Permute (Text => Text);
      end loop All_Rounds;

      Text := Text + Ks.Subkey ( (Last_Round + 1) / 4);
   end Encrypt;

   procedure Decrypt (Key_Schedule : in Key_Schedule_Handle; Text : in out Block) is
      function "-" (Left : Block; Right : Block) return Block; -- Word-by-word subtraction without borrow

      procedure Unmix (Round : in Round_Id; Side : in Eight_Id; Pair : in out Couple); -- Inverse of the Mix procedure

      procedure Permute (Text : in out Block); -- Inverse of the ecryption Permute procedure

      function "-" (Left : Block; Right : Block) return Block is
        (Left ( 0) - Right ( 0), Left ( 1) - Right ( 1), Left ( 2) - Right ( 2), Left ( 3) - Right ( 3),
         Left ( 4) - Right ( 4), Left ( 5) - Right ( 5), Left ( 6) - Right ( 6), Left ( 7) - Right ( 7),
         Left ( 8) - Right ( 8), Left ( 9) - Right ( 9), Left (10) - Right (10), Left (11) - Right (11),
         Left (12) - Right (12), Left (13) - Right (13), Left (14) - Right (14), Left (15) - Right (15) );

      procedure Unmix (Round : in Round_Id; Side : in Eight_Id; Pair : in out Couple) is
         -- Empty
      begin -- Unmix
         Pair (Pair'Last)  := Interfaces.Rotate_Right (Pair (Pair'Last) xor Pair (Pair'First), Rot (Round rem 8, Side) );
         Pair (Pair'First) := Pair (Pair'First) - Pair (Pair'Last);
      end Unmix;

      procedure Permute (Text : in out Block) is
         Temp9  : constant Word := Text ( 9);
         Temp13 : constant Word := Text (13);
         Temp6  : constant Word := Text ( 6);
         Temp11 : constant Word := Text (11);
         Temp15 : constant Word := Text (15);
         Temp10 : constant Word := Text (10);
         Temp12 : constant Word := Text (12);
         Temp14 : constant Word := Text (14);
      begin -- Permute
         Text ( 9) := Text (1);
         Text (13) := Text (3);
         Text ( 6) := Text (4);
         Text (11) := Text (5);
         Text ( 4) := Temp6;
         Text (15) := Text (7);
         Text (10) := Text (8);
         Text ( 7) := Temp9;
         Text (12) := Temp10;
         Text ( 3) := Temp11;
         Text (14) := Temp12;
         Text ( 5) := Temp13;
         Text ( 8) := Temp14;
         Text ( 1) := Temp15;
      end Permute;

      Ks : Key_Schedule_Handle renames Key_Schedule;
   begin -- Decrypt
      Text := Text - Ks.Subkey (Num_Rounds / 4);

      All_Rounds : for Round in reverse Round_Id loop
         Permute (Text => Text);
         Unmix (Round => Round, Side => Last,     Pair => Text (14 .. 15) );
         Unmix (Round => Round, Side => Seventh,  Pair => Text (12 .. 13) );
         Unmix (Round => Round, Side => Sixth,    Pair => Text (10 .. 11) );
         Unmix (Round => Round, Side => Fifth,    Pair => Text ( 8 ..  9) );
         Unmix (Round => Round, Side => Fourth,   Pair => Text ( 6 ..  7) );
         Unmix (Round => Round, Side => Third,    Pair => Text ( 4 ..  5) );
         Unmix (Round => Round, Side => Second,   Pair => Text ( 2 ..  3) );
         Unmix (Round => Round, Side => First,    Pair => Text ( 0 ..  1) );

         if Round rem 4 = 0 then
            Text := Text - Ks.Subkey (Round / 4);
         end if;
      end loop All_Rounds;
   end Decrypt;

   Bytes_Per_Block : constant := 128;

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
end PragmARC.Encryption.Threefish.Block_1024;
