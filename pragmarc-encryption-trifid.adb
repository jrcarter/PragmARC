-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2024 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Implementation of the Trifid cipher
--
-- 2024 Jun 15     J. Carter     V1.0--Initial version
--
with Ada.Characters.Handling;

package body PragmARC.Encryption.Trifid is
   type Coord_List is array (Positive range <>) of Index;

   type Coordinate is record
      Square : Index;
      Row    : Index;
      Col    : Index;
   end record;

   function Find (Letter : in Character; Key : in Cube) return Coordinate with
      Pre => Letter in Upper | '+';

   function Find (Letter : in Character; Key : in Cube) return Coordinate is
      -- Empty
   begin -- Find
      All_Squares : for S in Key'Range (1) loop
         All_Rows : for R in Key'Range (2) loop
            All_Cols : for C in Key'Range (3) loop
               if Key (S, R, C) = Letter then
                  return (Square => S, Row => R, Col => C);
               end if;
            end loop All_Cols;
         end loop All_Rows;
      end loop All_Squares;

      raise Program_Error with Letter & " not found in Key";
   end Find;

   function Encrypt (Plaintext : in String; Key : in Cube) return String is
      function Cleaned (Source : in String) return String with
         Post => Valid_Ciphertext (Cleaned'Result) and Cleaned'Result'First = 1;
      -- Converts Source to upper case and removes non-letters

      function Cleaned (Source : in String) return String is
         Capped : constant String := Ada.Characters.Handling.To_Upper (Source);

         Result : String (1 .. Source'Length);
         Last   : Natural := 0;
      begin -- Cleaned
         All_Source : for C of Capped loop
            case C is
            when Upper =>
               Last := Last + 1;
               Result (Last) := C;
            when others =>
               null;
            end case;
         end loop All_Source;

         return Result (1 .. Last);
      end Cleaned;

      Input : constant String := Cleaned (Plaintext);

      Loc    : Coordinate;
      Sqr    : Coord_List (Input'Range); -- Square in Key containing corresponding letter from Input
      Row    : Coord_List (Input'Range); -- Row in Key containing corresponding letter from Input
      Col    : Coord_List (Input'Range); -- Col in Key containing corresponding letter from Input
      Coord  : Coord_List (1 .. 3 * Input'Length); -- Sqr & Row & Col
      Result : String (Input'Range);
   begin -- Encrypt
      Get_Coords : for I in Input'Range loop
         Loc := Find (Input (I), Key);
         Sqr (I) := Loc.Square;
         Row (I) := Loc.Row;
         Col (I) := Loc.Col;
      end loop Get_Coords;

      Coord := Sqr & Row & Col;

      Convert : for I in Result'Range loop
         Result (I) := Key (Coord (3 * I - 2), Coord (3 * I - 1), Coord (3 * I) );
      end loop Convert;

      return Result;
   end Encrypt;

   function Decrypt (Ciphertext : in String; Key : in Cube) return String is
      Loc    : Coordinate;
      Coord  : Coord_List (1 .. 3 * Ciphertext'Length);
      Sqr    : Coord_List (Ciphertext'Range); -- Coord (                        1 .. Ciphertext'Length)
      Row    : Coord_List (Ciphertext'Range); -- Coord (    Ciphertext'Length + 1 .. 2 * Ciphertext'Length)
      Col    : Coord_List (Ciphertext'Range); -- Coord (2 * Ciphertext'Length + 1 .. Coord'Last)
      Result : String (Ciphertext'Range);
   begin -- Decrypt
      Get_Coords : for I in Ciphertext'Range loop
         Loc := Find (Ciphertext (I), Key);
         Coord (3 * I - 2) := Loc.Square;
         Coord (3 * I - 1) := Loc.Row;
         Coord (3 * I) := Loc.Col;
      end loop Get_Coords;

      Sqr := Coord (                        1 .. Ciphertext'Length);
      Row := Coord (    Ciphertext'Length + 1 .. 2 * Ciphertext'Length);
      Col := Coord (2 * Ciphertext'Length + 1 .. Coord'Last);

      Convert : for I in Result'Range loop
         Result (I) := Key (Sqr (I), Row (I), Col (I) );
      end loop Convert;

      return Result;
   end Decrypt;
end PragmARC.Encryption.Trifid;
