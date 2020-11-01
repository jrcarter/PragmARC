-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Fast "hashing function specifically tailored to variable-length text strings."
-- "Similar strings are not likely to collide."
-- From P. K. Pearson, "Fast Hashing of Variable-Length Text Strings," Comm. ACM, 1990 Jun
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed formatting
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

package PragmARC.Hash_Fast_Variable_Length with Pure is
   type Byte is mod 2 ** 8 with Size => 8; -- The hashing function returns a Byte

   type Permutation_Table is array (Byte) of Byte;
   -- A permutation table should contain a random permutation of the values 0 .. 255

   Default_Table : constant Permutation_Table :=
      Permutation_Table'(001, 087, 049, 012, 176, 178, 102, 166, 121, 193, 006, 084, 249, 230, 044,163,
                         014, 197, 213, 181, 161, 085, 218, 080, 064, 239, 024, 226, 236, 142, 038, 200,
                         110, 177, 104, 103, 141, 253, 255, 050, 077, 101, 081, 018, 045, 096, 031, 222,
                         025, 107, 190, 070, 086, 237, 240, 034, 072, 242, 020, 214, 244, 227, 149, 235,
                         097, 234, 057, 022, 060, 250, 082, 175, 208, 005, 127, 199, 111, 062, 135, 248,
                         174, 169, 211, 058, 066, 154, 106, 195, 245, 171, 017, 187, 182, 179, 000, 243,
                         132, 056, 148, 075, 128, 133, 158, 100, 130, 126, 091, 013, 153, 246, 216, 219,
                         119, 068, 223, 078, 083, 088, 201, 099, 122, 011, 092, 032, 136, 114, 052, 010,
                         138, 030, 048, 183, 156, 035, 061, 026, 143, 074, 251, 094, 129, 162, 063, 152,
                         170, 007, 115, 167, 241, 206, 003, 150, 055, 059, 151, 220, 090, 053, 023, 131,
                         125, 173, 015, 238, 079, 095, 089, 016, 105, 137, 225, 224, 217, 160, 037, 123,
                         118, 073, 002, 157, 046, 116, 009, 145, 134, 228, 207, 212, 202, 215, 069, 229,
                         027, 188, 067, 124, 168, 252, 042, 004, 029, 108, 021, 247, 019, 205, 039, 203,
                         233, 040, 186, 147, 198, 192, 155, 033, 164, 191, 098, 204, 165, 180, 117, 076,
                         140, 036, 210, 172, 041, 054, 159, 008, 185, 232, 113, 196, 231, 047, 146, 120,
                         051, 065, 028, 144, 254, 221, 093, 189, 194, 139, 112, 043, 071, 109, 184, 209);
   -- This permutation table gives good hashing behavior

   generic -- Hash
      type Element is limited private; -- "Characters" which make up a string
      type Index is (<>); -- Index type of the string type
      type String is array (Index range <>) of Element;

      with function To_Byte (Item : Element) return Byte;
   function Hash (Item : String; Table : Permutation_Table := Default_Table) return Byte;
end PragmARC.Hash_Fast_Variable_Length;
