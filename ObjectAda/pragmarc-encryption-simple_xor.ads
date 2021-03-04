-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of the XOR cipher
--
-- 2021 Feb 01     J. Carter     V1.0--Initial version
--
package PragmARC.Encryption.Simple_XOR is
   type Byte_Value is mod 2 ** 8;

   type Byte_List is array (Positive range <>) of Byte_Value;

   function Crypt (Text : in Byte_List; Key : in Byte_List) return Byte_List with
      Post => Crypt'Result'First = Text'First and Crypt'Result'Last = Text'Last;
   -- If Text is plaintext, encrypts Text with Key
   -- If Text is ciphertext encrypted with Key, decrypts Text
   -- Crypt (Crypt (T, K), K) = Text

   function To_Bytes (Source : in String) return Byte_List with
      Post => Source'Length = To_Bytes'Result'Length;
   -- Help function for using a string as a key

   procedure Crypt (Input_Name : in String; Output_Name : in String; Key : in Byte_List);
   -- Opens Input_Name and creates Output_Name
   -- Encrypts the contents of Input_Name into Output_Name
   -- Any exceptions opening or creating the files are propagated to the caller
end PragmARC.Encryption.Simple_XOR;

