-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of the Bifid cipher
--
-- 2023 May 15     J. Carter     V1.0--Initial version
--
package PragmARC.Encryption.Bifid is
   subtype Index is Integer range 1 .. 5;
   subtype Upper is Character range 'A' .. 'Z';

   function Valid_Ciphertext (Source : in String) return Boolean is
      (for all C of Source => C in Upper and C /= 'J');

   subtype Square_Row is String (Index) with Dynamic_Predicate => Valid_Ciphertext (Square_Row);
   type Square_Layout is array (Index) of Square_Row with
      Dynamic_Predicate => (for all C in Upper =>
                               (if C = 'J' then True else (for some Row of Square_Layout => (for some L of Row => L = C) ) ) );
   -- Each Character in Upper except 'J' must appear in a Square_Layout exactly once

   function Encrypt (Plaintext : in String; Key : in Square_Layout) return String with Post => Valid_Ciphertext (Encrypt'Result);
   -- Converts Plaintext to upper case, removes non-letters, and changes 'J' to 'I', then encrypts that with Key

   function Decrypt (Ciphertext : in String; Key : in Square_Layout) return String with
      Pre  => Valid_Ciphertext (Ciphertext),
      Post => Valid_Ciphertext (Decrypt'Result);
   -- Decrypts Ciphertext with Key
end PragmARC.Encryption.Bifid;
