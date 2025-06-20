-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- Implementation of the Trifid cipher
--
-- History:
-- 2025 Jul 01     J. Carter     V1.2--Use SPDX license format
-- 2024 Jun 15     J. Carter     V1.0--Initial version
--
package PragmARC.Encryption.Trifid is
   subtype Index is Integer range 1 .. 3;
   subtype Upper is Character range 'A' .. 'Z';

   function Valid_Ciphertext (Source : in String) return Boolean is
      ( (for all C of Source => C in Upper | '+') and Source'First = 1);

   type Cube is array (Index, Index, Index) of Character with
      Dynamic_Predicate => (for all C in Upper => (for some L of Cube => L = C) ) and
                           (for some L of Cube => L = '+');
   -- Each Character in Upper must appear in a Cube exactly once, as must '+'

   function Encrypt (Plaintext : in String; Key : in Cube) return String with Post => Valid_Ciphertext (Encrypt'Result);
   -- Converts Plaintext to upper case and removes non-letters, then encrypts that with Key

   function Decrypt (Ciphertext : in String; Key : in Cube) return String with
      Pre  => Valid_Ciphertext (Ciphertext),
      Post => Valid_Ciphertext (Decrypt'Result);
   -- Decrypts Ciphertext with Key
end PragmARC.Encryption.Trifid;
