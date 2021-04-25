-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Data_Structures.Sets.Discrete is
   function "+" (Left : in Set; Right : in Set) return Set is
      (Value => Left.Value or Right.Value);

   function "+" (Left : in Set; Right : in Element) return Set is
      Result : Set := Left;
   begin -- "+"
      Result.Value (Right) := True;

      return Result;
   end "+";

   function "+" (Left : in Element; Right : in Set) return Set is
      Result : Set := Right;
   begin -- "+"
      Result.Value (Left) := True;

      return Result;
   end "+";

   function "*" (Left : in Set; Right : in Set) return Set is
      (Value => Left.Value and Right.Value);

   function "-" (Left : in Set; Right : in Set) return Set is
     (Value => Left.Value and not Right.Value);

   function "-" (Left : in Set; Right : in Element) return Set is
      Result : Set := Left;
   begin -- "-"
      Result.Value (Right) := False;

      return Result;
   end "-";

   function "/" (Left : in Set; Right : in Set) return Set is
      (Value => Left.Value xor Right.Value);

   function "<=" (Left : in Set; Right : in Set) return Boolean is
      ( (Left.Value and Right.Value) = Left.Value);

   function "<" (Left : in Set; Right : in Set) return Boolean is
      (Left.Value /= Right.Value and then (Left.Value and Right.Value) = Left.Value);

   function ">=" (Left : in Set; Right : in Set) return Boolean is
      ( (Left.Value and Right.Value) = Right.Value);

   function ">" (Left : in Set; Right : in Set) return Boolean is
      (Left.Value /= Right.Value and then (Left.Value and Right.Value) = Right.Value);

   function Member (Group : in Set; Item : in Element) return Boolean is
      (Group.Value (Item) );

   function New_Set (List : Member_List) return Set is
      Result : Set;
   begin -- New_Set
      All_Items : for Item of List loop
         Result.Value (Item) := True;
      end loop All_Items;

      return Result;
   end New_Set;

   function Size (Group : in Set) return Natural is
      Result : Natural := 0;
   begin -- Size
      Count : for B of Group.Value loop
         Result := Result + Boolean'Pos (B);
      end loop Count;

      return Result;
   end Size;
end PragmARC.Data_Structures.Sets.Discrete;
