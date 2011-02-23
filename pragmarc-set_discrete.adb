-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Set_Discrete is
   function "+" (Left : Set; Right : Set) return Set is
      -- null;
   begin -- "+"
      return Set'(Value => Left.Value or Right.Value);
   end "+";

   function "+" (Left : Set; Right : Element) return Set is
      Result : Set := Left;
   begin -- "+"
      Result.Value (Right) := True;

      return Result;
   end "+";

   function "+" (Left : Element; Right : Set) return Set is
      Result : Set := Right;
   begin -- "+"
      Result.Value (Left) := True;

      return Result;
   end "+";

   function "*" (Left : Set; Right : Set) return Set is
      -- null;
   begin -- "*"
      return Set'(Value => Left.Value and Right.Value);
   end "*";

   function "-" (Left : Set; Right : Set) return Set is
      -- null;
   begin -- "-"
      return Set'(Value => Left.Value and not Right.Value);
   end "-";

   function "-" (Left : Set; Right : Element) return Set is
      Result : Set := Left;
   begin -- "-"
      Result.Value (Right) := False;

      return Result;
   end "-";

   function "/" (Left : Set; Right : Set) return Set is
      -- null;
   begin -- "/"
      return Set'(Value => Left.Value xor Right.Value);
   end "/";

   function "<=" (Left : Set; Right : Set) return Boolean is
      -- null;
   begin -- "<="
      return (Left.Value and Right.Value) = Left.Value;
   end "<=";

   function "<" (Left : Set; Right : Set) return Boolean is
      -- null;
   begin -- "<"
      return Left.Value /= Right.Value and then (Left.Value and Right.Value) = Left.Value;
   end "<";

   function ">=" (Left : Set; Right : Set) return Boolean is
      -- null;
   begin -- ">="
      return (Left.Value and Right.Value) = Right.Value;
   end ">=";

   function ">" (Left : Set; Right : Set) return Boolean is
      -- null;
   begin -- ">"
      return Left.Value /= Right.Value and then (Left.Value and Right.Value) = Right.Value;
   end ">";

   function Member (Item : Element; Group : Set) return Boolean is
      -- null;
   begin -- Member
      return Group.Value (Item);
   end Member;

   function Make_Set (List : Member_List) return Set is
      Result : Set;
   begin -- Make_Set
      All_Items : for Item in List'Range loop
         Result.Value (List (Item) ) := True;
      end loop All_Items;

      return Result;
   end Make_Set;

   function Size (Group : Set) return Natural is
      Result : Natural := 0;
   begin -- Size
      Count : for I in Group.Value'range loop
         Result := Result + Boolean'Pos (Group.Value (I) );
      end loop Count;

      return Result;
   end Size;
end PragmARC.Set_Discrete;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.