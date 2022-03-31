-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Divides Key into Bytes_Per_Hash substrings and hashes each with Hash_Fast_Variable_Length,
-- combining the results into a Hash_Type
--
-- History:
-- 2022 Apr 01     J. Carter          V1.0--Initial version
--
with Ada.Unchecked_Conversion;
with PragmARC.Hash_Fast_Variable_Length;
with System;

function PragmARC.Hash (Key : in String) return Ada.Containers.Hash_Type is
   subtype Byte      is PragmARC.Hash_Fast_Variable_Length.Byte;
   subtype Hash_Type is Ada.Containers.Hash_Type;

   Bytes_Per_Hash : constant := Hash_Type'Size / Byte'Size;

   type Hash_As_Bytes is array (1 ..  Bytes_Per_Hash) of Byte With Component_Size => Byte'Size, Size => Hash_Type'Size;
   -- 1 => LSB, Bytes_Per_Hash => MSB

   function To_Hash (List : in Hash_As_Bytes) return Hash_Type;
   -- Endian-independent conversion

   function To_Hash (List : in Hash_As_Bytes) return Hash_Type is
      procedure Reverse_Bytes (List : in out Hash_As_Bytes);
      -- Reverses the bytes of List

      function From_Bytes is new Ada.Unchecked_Conversion (Source => Hash_As_Bytes, Target => Hash_Type);

      procedure Reverse_Bytes (List : in out Hash_As_Bytes) is
         procedure Swap (Left : in out Byte; Right : in out Byte);
         -- Swaps Left and Right

         procedure Swap (Left : in out Byte; Right : in out Byte) is
            Temp : constant Byte := Left;
         begin -- Swap
            Left := Right;
            Right := Temp;
         end Swap;

         Last : Natural := List'Last;
      begin -- Reverse_Bytes
         Swap_All : for I in List'First .. Last / 2 loop
            Swap (Left => List (I), Right => List (Last) );
            Last := Last - 1;
         end loop Swap_All;
      end Reverse_Bytes;

      Local : Hash_As_Bytes := List;

      use type System.Bit_Order;
   begin -- To_Hash
      if System.Default_Bit_Order = System.High_Order_First then
         Reverse_Bytes (List => Local);
      end if;

      return From_Bytes (Local);
   end To_Hash;

   function To_Byte (Item : in Character) return Byte is (Character'Pos (Item) );

   function Short_Hash is new PragmARC.Hash_Fast_Variable_Length.Hash
      (Element => Character, Index => Positive, String => String, To_Byte => To_Byte);

   Result : Hash_As_Bytes;
begin -- Hash
   if Key'Length <= Bytes_Per_Hash then -- Each substring is at most 1 character long
      All_Characters : for I in Key'Range loop
         Result (I - Key'First + 1) := Short_Hash (Key (I .. I) );
      end loop All_Characters;

      Result (Key'Length + 1 .. Result'Last) := (others => Short_Hash ("") );

      return To_Hash (Result);
   end if;

   Long_Key : declare
      Length : constant Natural := Key'Length / Bytes_Per_Hash; -- Each substring is Length characters long except the last,
                                                                -- which is Length + Key'Length rem Bytes_Per_Hash characters long
      Start : Positive := Key'First;
      Stop  : Positive;
   begin -- Long_Key
      All_Bytes : for I in Result'Range loop
         Stop := (if I = Result'Last then Key'Last else Start + Length - 1);
         Result (I) := Short_Hash (Key (Start .. Stop) );
         Start := Stop + 1;
      end loop All_Bytes;
   end Long_Key;

   return To_Hash (Result);
end PragmARC.Hash;
