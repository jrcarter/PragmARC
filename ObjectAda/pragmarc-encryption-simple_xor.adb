-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- 2021 Feb 01     J. Carter     V1.0--Initial version
--
with Ada.Sequential_IO;
with Ada.Unchecked_Conversion;

package body PragmARC.Encryption.Simple_XOR is
   function Crypt (Text : in Byte_List; Key : in Byte_List) return Byte_List is
      Result : Byte_List := Text;
      Index  : Positive  := Key'First;
   begin -- Crypt
      All_Bytes : for B of Result loop
         B := B xor Key (Index);
         Index := (if Index >= Key'Last then Key'First else Index + 1);
      end loop All_Bytes;

      return Result;
   end Crypt;

   function To_Bytes (Source : in String) return Byte_List is
      subtype S_String is String    (Source'Range);
      subtype B_List   is Byte_List (Source'Range);

      function As_Bytes is new Ada.Unchecked_Conversion (Source => S_String, Target => B_List);
   begin -- To_Bytes
      return As_Bytes (Source);
   end To_Bytes;

   procedure Crypt (Input_Name : in String; Output_Name : in String; Key : in Byte_List) is
      package Byte_IO is new Ada.Sequential_IO (Element_Type => Byte_Value);

      subtype Key_List is Byte_List (1 .. Key'Length);

      procedure Get (Input : in Byte_IO.File_Type; List : out Key_List; Last : out Natural) with
         Pre  => Byte_IO.Is_Open (Input) and then Byte_IO.Mode (Input) in Byte_IO.In_File,
         Post => Last <= List'Last;
      -- Reads bytes from Input until List is filled or End_Of_File (Input)
      -- Last is the index in List of the last position filled, or zero if Input was already at EOF

      procedure Put (Output : in Byte_IO.File_Type; List : in Byte_List) with
         Pre => Byte_IO.Is_Open (Output) and then Byte_IO.Mode (Output) in Byte_IO.Out_File;
      -- Writes the bytes of List to Output

      procedure Get (Input : in Byte_IO.File_Type; List : out Key_List; Last : out Natural) is
         -- Empty
      begin -- Get
         Last := 0;

         Read : for I in List'Range loop
            exit Read when Byte_IO.End_Of_File (Input);

            Byte_IO.Read (File => Input, Item => List (I) );
            Last := I;
         end loop Read;
      end Get;

      procedure Put (Output : in Byte_IO.File_Type; List : in Byte_List) is
         -- Empty
      begin -- Put
         Write : for B of List loop
            Byte_IO.Write (File => Output, Item => B);
         end loop Write;
      end Put;

      Input  : Byte_IO.File_Type;
      Output : Byte_IO.File_Type;
      Block  : Key_List;
      Last   : Natural;
   begin -- Crypt
      Byte_IO.Open (File => Input, Mode => Byte_IO.In_File, Name => Input_Name);
      Byte_IO.Create (File => Output, Mode => Byte_IO.Out_File, Name => Output_Name);

      All_Blocks : loop
         exit All_Blocks when Byte_IO.End_Of_File (Input);

         Get (Input => Input, List => Block, Last => Last);
         Put (Output => Output, List => Crypt (Block (1 .. Last), Key) );

         exit All_Blocks when Last < Block'Last;
      end loop All_Blocks;

      Byte_IO.Close (File => Input);
      Byte_IO.Close (File => Output);
   end Crypt;
end PragmARC.Encryption.Simple_XOR;

