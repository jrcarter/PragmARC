-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of the Bifid cipher
--
-- 2023 May 15     J. Carter     V1.0--Initial version
--
with Ada.Characters.Handling;

package body PragmARC.Encryption.Bifid is
   type Coord_List is array (Positive range <>) of Index;

   type Coordinate is record
      Row : Index;
      Col : Index;
   end record;

   function Find (Letter : in Upper; Key : in Square_Layout) return Coordinate with Pre => Letter /= 'J';

   function Find (Letter : in Upper; Key : in Square_Layout) return Coordinate is
   begin -- Find
      All_Rows : for R in Key'Range loop
         All_Cols : for C in Key (R)'Range loop
            if Key (R) (C) = Letter then
               return (Row => R, Col => C);
            end if;
         end loop All_Cols;
      end loop All_Rows;

      raise Program_Error with Letter & " not found in Key";
   end Find;

   function Encrypt (Plaintext : in String; Key : in Square_Layout) return String is
      function Cleaned (Source : in String) return String with
         Post => Valid_Ciphertext (Cleaned'Result) and Cleaned'Result'First = 1;
   -- Converts Source to upper case, removes non-letters, and changes 'J' to 'I'

      function Cleaned (Source : in String) return String is
         Capped : constant String := Ada.Characters.Handling.To_Upper (Source);

         Result : String (1 .. Source'Length);
         Last   : Natural := 0;
      begin -- Cleaned
         All_Source : for C of Capped loop
            case C is
            when Upper =>
               Last := Last + 1;
               Result (Last) := (if C = 'J' then 'I' else C);
            when others =>
               null;
            end case;
         end loop All_Source;

         return Result (1 .. Last);
      end Cleaned;

      Input : constant String := Cleaned (Plaintext);

      Loc    : Coordinate;
      Row    : Coord_List (Input'Range); -- Row in Key containing corresponding letter from Input
      Col    : Coord_List (Input'Range); -- Col in Key containing corresponding letter from Input
      Coord  : Coord_List (1 .. 2 * Input'Length); -- Row & Col
      Result : String (Input'Range);
   begin -- Encrypt
      Get_Coords : for I in Input'Range loop
         Loc := Find (Input (I), Key);
         Row (I) := Loc.Row;
         Col (I) := Loc.Col;
      end loop Get_Coords;

      Coord := Row & Col;

      Convert : for I in Result'Range loop
         Result (I) := Key (Coord (2 * I - 1) ) (Coord (2 * I) );
      end loop Convert;

      return Result;
   end Encrypt;

   function Decrypt (Ciphertext : in String; Key : in Square_Layout) return String is
      Loc    : Coordinate;
      Coord  : Coord_List (1 .. 2 * Ciphertext'Length);
      Row    : Coord_List (Ciphertext'Range); -- Coord (1 .. Ciphertext'Length)
      Col    : Coord_List (Ciphertext'Range); -- Coord (Ciphertext'Length + 1 .. Coord'Last)
      Result : String (Ciphertext'Range);
   begin -- Decrypt
      Get_Coords : for I in Ciphertext'Range loop
         Loc := Find (Ciphertext (I), Key);
         Coord (2 * I - 1) := Loc.Row;
         Coord (2 * I) := Loc.Col;
      end loop Get_Coords;

      Row := Coord (1 .. Ciphertext'Length);
      Col := Coord (Ciphertext'Length + 1 .. Coord'Last);

      Convert : for I in Result'Range loop
         Result (I) := Key (Row (I) ) (Col (I) );
      end loop Convert;

      return Result;
   end Decrypt;
end PragmARC.Encryption.Bifid;
