-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Hash_Fast_Variable_Length is
   function Hash (Item : in String; Table : in Permutation_Table := Default_Table) return Byte is
      Result : Byte := 0;
   begin -- Hash
      All_Elements : for I in Item'Range loop
         Result := Table (To_Byte (Item (I) ) xor Result);
      end loop All_Elements;

      return Result;
   end Hash;
end PragmARC.Hash_Fast_Variable_Length;
