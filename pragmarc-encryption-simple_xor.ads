-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Implementation of the XOR cipher
--
-- History:
-- 2025 Aug 15     J. Carter     V1.2--Common definition of a byte
-- 2025 Jul 01     J. Carter     V1.1--Use SPDX license format
-- 2021 Feb 01     J. Carter     V1.0--Initial version
--
package PragmARC.Encryption.Simple_XOR is
   function Crypt (Text : in Byte_List; Key : in Byte_List) return Byte_List with
      Post => Crypt'Result'First = Text'First and Crypt'Result'Last = Text'Last;
   -- If Text is plaintext, encrypts Text with Key
   -- If Text is ciphertext encrypted with Key, decrypts Text
   -- Crypt (Crypt (T, K), K) = T

   function To_Bytes (Source : in String) return Byte_List with
      Post => Source'Length = To_Bytes'Result'Length;
   -- Help function for using a string as a key

   procedure Crypt (Input_Name : in String; Output_Name : in String; Key : in Byte_List);
   -- Opens Input_Name and creates Output_Name
   -- Encrypts the contents of Input_Name into Output_Name
   -- Any exceptions opening or creating the files are propagated to the caller
end PragmARC.Encryption.Simple_XOR;
