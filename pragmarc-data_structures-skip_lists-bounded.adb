-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
-- **************************************************************************
--
-- History:
-- 2026 Aug 01     J. Carter          V1.0--Initial release
--
with Ada.Numerics.Float_Random;

package body PragmARC.Data_Structures.Skip_Lists.Bounded is
   use Ada.Numerics;

   Gen : Float_Random.Generator;

   function Initial_List (Length : in Positive) return Node_List is
      Result : Node_List (0 .. Length);
   begin -- Initial_List
      Fill_Free : for I in Result'Range loop
         Result (I).Level := 1;
         Result (I).Forward := (if I in 0 | Length then (Level_ID => 0) else (1 => I + 1, others => 0) );
      end loop Fill_Free;

      return Result;
   end Initial_List;

   procedure Clear (List : in out Skip_List) is
      -- Empty
   begin -- Clear
      List := (Max_Length => List.Max_Length,
               Level      => Level_ID'First,
               List       => Initial_List (List.Max_Length),
               Last       => 0,
               Length     => 0,
               Free       => 1);
   end Clear;

   procedure Assign (To : in out Skip_List; From : in Skip_List) is
      procedure Add_One (Item : in Element);
      -- Inserts Item into To

      procedure Add_All is new Iterate (Action => Add_One);

      procedure Add_One (Item : in Element) is
         -- Empty
      begin -- Add_One
         To.Insert (Item => Item);
      end Add_One;
   begin -- Assign
      if To.Max_Length = From.Max_Length then
         To := From;

         return;
      end if;

      To.Clear;
      Add_All (List => From);
   end Assign;

   function Search (List : in Skip_List; Item : in Element) return Result is
      Ptr : Natural := 0;
   begin -- Search
      All_Levels : for I in reverse Level_ID'First .. List.Level loop
         Advance : loop
            exit Advance when List.List (Ptr).Forward (I) = 0 or else not (List.List (List.List (Ptr).Forward (I) ).Value < Item);

            Ptr := List.List (Ptr).Forward (I);
         end loop Advance;
      end loop All_Levels;

      Ptr := List.List (Ptr).Forward (Level_ID'First);

      if Ptr = 0 or else List.List (Ptr).Value /= Item then
         return Result'(Found => False);
      end if;

      return Result'(Found => True, Item => List.List (Ptr).Value);
   end Search;

   procedure Insert (List : in out Skip_List; Item : in Element) is
      Update : Forward_Set := Forward_Set'(Level_ID => 0);
      Ptr    : Natural     := 0;

      function Random_Level (List_Level : in Level_ID) return Level_ID;
      -- Obtain a random level for the new node, possibly increasing the level of List by 1

      function Random_Level (List_Level : in Level_ID) return Level_ID is
         Probability : constant := 0.25;

         New_Level : Level_ID := Level_ID'First;
      begin -- Random_Level
         Increment : loop
            exit Increment when Float_Random.Random (Gen) >= Probability or New_Level >= Max_Level or New_Level >= List_Level + 1;

            New_Level := New_Level + 1;
         end loop Increment;

         return New_Level;
      end Random_Level;
   begin -- Insert
      All_Levels : for I in reverse Level_ID'First .. List.Level loop
         Advance : loop
            exit Advance when List.List (Ptr).Forward (I) = 0 or else not (List.List (List.List (Ptr).Forward (I) ).Value < Item);

            Ptr := List.List (Ptr).Forward (I);
         end loop Advance;

         Update (I) := Ptr;
      end loop All_Levels;

      Ptr := List.List (Ptr).Forward (Level_ID'First);

      if Ptr /= 0 and then List.List (Ptr).Value = Item then
         List.List (Ptr).Value := Item;
      else
         if List.Free = 0 then
            raise Too_Short;
         end if;

         Ptr := List.Free;
         List.Free := List.List (Ptr).Forward (1);
         List.List (Ptr).Level := Random_Level (List.Level);
         List.List (Ptr).Value := Item;
         List.Length := List.Length + 1;

         if List.List (Ptr).Level > List.Level then
            List.Level := List.List (Ptr).Level;
         end if;

         Adjust_Links : for I in Level_ID'First .. List.List (Ptr).Level loop
            List.List (Ptr).Forward (I) := List.List (Update (I) ).Forward (I);
            List.List (Update (I) ).Forward (I) := Ptr;
         end loop Adjust_Links;

         if List.List (Ptr).Forward (Level_ID'First) = 0 then -- New last Node
            List.Last := Ptr;
         end if;
      end if;
   end Insert;

   procedure Delete (List : in out Skip_List; Item : in Element) is
      Update : Forward_Set := Forward_Set'(Level_ID => 0);
      Ptr    : Natural     := 0;
   begin -- Delete
      All_Levels : for I in reverse Level_ID'First .. List.Level loop
         Advance : loop
            exit Advance when List.List (Ptr).Forward (I) = 0 or else not (List.List (List.List (Ptr).Forward (I) ).Value < Item);

            Ptr := List.List (Ptr).Forward (I);
         end loop Advance;

         Update (I) := Ptr;
      end loop All_Levels;

      Ptr := List.List (Ptr).Forward (Level_ID'First);

      if Ptr /= 0 and then List.List (Ptr).Value = Item then
         Adjust_Links : for I in Level_ID'First .. List.Level loop
            exit Adjust_Links when List.List (Update (I) ).Forward (I) /= Ptr;

            List.List (Update (I) ).Forward (I) := List.List (Ptr).Forward (I);
         end loop Adjust_Links;

         Adjust_Level : loop
            exit Adjust_Level when List.Level <= Level_ID'First or else List.List (0).Forward (List.Level) /= 0;

            List.Level := List.Level - 1;
         end loop Adjust_Level;

         if List.Last = Ptr then -- Deleted Node at end of List
            List.Last := Update (Level_ID'First);
         end if;

         List.Length := List.Length - 1;
         List.List (Ptr).Forward (1) := List.Free;
         List.Free := Ptr;
      end if;
   end Delete;

   function Get_First (List : in Skip_List) return Element is
      (List.List (List.List (0).Forward (Level_ID'First) ).Value);

   function Get_Last (List : in Skip_List) return Element is
      (List.List (List.Last).Value);

   function Is_Empty (List : in Skip_List) return Boolean is
      (List.Last = 0);

   function Length (List : in Skip_List) return Natural is
      (List.Length);

   procedure Iterate (List : in Skip_List) is
      Ptr: Natural := List.List (0).Forward (Level_ID'First);
   begin -- iterate
      All_Nodes : loop
         exit All_Nodes when Ptr = 0;

         Action (Item => List.List (Ptr).Value);
         Ptr := List.List (Ptr).Forward (Level_ID'First);
      end loop All_Nodes;
   end Iterate;
begin -- PragmARC.Data_Structures.Skip_Lists.Bounded
   Float_Random.Reset (Gen);
end PragmARC.Data_Structures.Skip_Lists.Bounded;
