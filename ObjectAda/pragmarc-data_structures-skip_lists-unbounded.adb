-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2018 Aug 01     J. Carter          V1.3--Make Length O(1)
-- 2016 Jun 01     J. Carter          V1.2--Changed comment for empty declarative part
-- 2013 Oct 01     J. Carter          V1.1--Added exception handler to Finalize
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.4--Added Context to Iterate; use mode out to allow scalars
-- 2002 May 01     J. Carter          V1.3--Added Assign
-- 2001 May 01     J. Carter          V1.2--Added Is_Empty; improved memory usage
-- 2000 Jul 01     J. Carter          V1.1--Changed to use Ada.Numerics.Float_Random
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;

package body PragmARC.Data_Structures.Skip_Lists.Unbounded is
   use Ada.Numerics;

   Gen : Float_Random.Generator;

   procedure Dispose is new Ada.Unchecked_Deallocation (Object => Node, Name => Link);

   procedure Clear (List : in out Skip_List) is
      Ptr : Link;
   begin -- Clear
      Remove_All : loop
         exit Remove_All when List.Header.Forward (Level_Id'First) = null;

         Ptr := List.Header.Forward (Level_Id'First);
         List.Header.Forward (Level_Id'First) := Ptr.Forward (Level_Id'First);
         Dispose (X => Ptr);
      end loop Remove_All;

      List.Header.Forward := Forward_Set'(Level_Id => null);
      List.Last := null;
      List.Level := Level_Id'First;
      List.Length := 0;
   end Clear;

   procedure Assign (To : out Skip_List; From : in Skip_List) is
      Ptr : Link := From.Header.Forward (Level_Id'First);
   begin -- Assign
      if To.Header = From.Header then
         return; -- These are the same lists
      end if;

      Clear (List => To);

      if From.Last = null then -- From is empty
         return;
      end if;

      Copy : loop
         exit Copy when Ptr = null;

         Insert (List => To, Item => Ptr.Value);
         Ptr := Ptr.Forward (Level_Id'First);
      end loop Copy;
   end Assign;

   function Search (List : Skip_List; Item : Element) return Result is
      Ptr : Link := List.Header;
   begin -- Search
      All_Levels : for I in reverse Level_Id'First .. List.Level loop
         Advance : loop
            exit Advance when Ptr.Forward (I) = null or else not (Ptr.Forward (I).Value < Item);

            Ptr := Ptr.Forward (I);
         end loop Advance;
      end loop All_Levels;

      Ptr := Ptr.Forward (Level_Id'First);

      if Ptr = null or else Ptr.Value /= Item then
         return Result'(Found => False);
      end if;

      return Result'(Found => True, Item => Ptr.Value);
   end Search;

   procedure Insert (List : in out Skip_List; Item : in Element) is
      New_Level : Level_Id;
      Update    : Forward_Set (Level_Id) := Forward_Set'(Level_Id => List.Header);
      Ptr       : Link                   := List.Header;

      function Random_Level (List_Level : Level_Id) return Level_Id is
         Probability : constant := 0.25;

         New_Level : Level_Id := Level_Id'First;
      begin -- Random_Level
         Increment : loop
            exit Increment when Float_Random.Random (Gen) >= Probability or New_Level >= Max_Level or New_Level >= List_Level + 1;

            New_Level := New_Level + 1;
         end loop Increment;

         return New_Level;
      end Random_Level;
   begin -- Insert
      All_Levels : for I in reverse Level_Id'First .. List.Level loop
         Advance : loop
            exit Advance when Ptr.Forward (I) = null or else not (Ptr.Forward (I).Value < Item);

            Ptr := Ptr.Forward (I);
         end loop Advance;

         Update (I) := Ptr;
      end loop All_Levels;

      Ptr := Ptr.Forward (Level_Id'First);

      if Ptr /= null and then Ptr.Value = Item then
         Ptr.Value := Item;
      else
         New_Level := Random_Level (List.Level);
         Ptr := new Node (Has_Data => True, Level => New_Level);

         Ptr.Value := Item;
         List.Length := List.Length + 1;

         if New_Level > List.Level then
            List.Level := New_Level;
         end if;

         Adjust_Links : for I in Level_Id'First .. New_Level loop
            Ptr.Forward (I) := Update (I).Forward (I);
            Update (I).Forward (I) := Ptr;
         end loop Adjust_Links;

         if Ptr.Forward (Level_Id'First) = null then -- New last Node
            List.Last := Ptr;
         end if;
      end if;
   exception -- Insert
   when Storage_Error =>
      raise Storage_Exhausted;
   end Insert;

   procedure Delete (List : in out Skip_List; Item : in Element) is
      Update : Forward_Set (Level_Id) := Forward_Set'(Level_Id => List.Header);
      Ptr    : Link                   := List.Header;
   begin -- Delete
      All_Levels : for I in reverse Level_Id'First .. List.Level loop
         Advance : loop
            exit Advance when Ptr.Forward (I) = null or else not (Ptr.Forward (I).Value < Item);

            Ptr := Ptr.Forward (I);
         end loop Advance;

         Update (I) := Ptr;
      end loop All_Levels;

      Ptr := Ptr.Forward (Level_Id'First);

      if Ptr /= null and then Ptr.Value = Item then
         Adjust_Links : for I in Level_Id'First .. List.Level loop
            exit Adjust_Links when Update (I).Forward (I) /= Ptr;

            Update (I).Forward (I) := Ptr.Forward (I);
         end loop Adjust_Links;

         Adjust_Level : loop
            exit Adjust_Level when List.Level <= Level_Id'First or else List.Header.Forward (List.Level) /= null;

            List.Level := List.Level - 1;
         end loop Adjust_Level;

         if List.Last = Ptr then -- Deleted Node at end of List
            List.Last := Update (Level_Id'First);

            if List.Last = List.Header then -- This deletion emptied the List
               List.Last := null;
            end if;
         end if;

         Dispose (X => Ptr);
         List.Length := List.Length - 1;
      end if;
   end Delete;

   function Get_First (List : Skip_List) return Element is
      (List.Header.Forward (Level_Id'First).Value);

   function Get_Last (List : Skip_List) return Element is
      (List.Last.Value);

   function Is_Empty (List : Skip_List) return Boolean is
      (List.Last = null);

   function Length (List : Skip_List) return Natural is
      (List.Length);

   procedure Finalize (Object : in out Skip_List) is
      -- Empty
   begin -- Finalize
      if Object.Header /= null then
         Clear (List => Object);
      end if;

      Dispose (X => Object.Header);
      Object.Last := null;
   exception -- Finalize
   when others =>
      null;
   end Finalize;

   procedure Iterate (List : in out Skip_List) is
      Ptr: Link := List.Header.Forward (Level_Id'First);
   begin -- iterate
      All_Nodes : loop
         exit All_Nodes when Ptr = null;

         Action (Item => Ptr.Value);
         Ptr := Ptr.Forward (Level_Id'First);
      end loop All_Nodes;
   end Iterate;
begin -- PragmARC.Data_Structures.Skip_Lists.Unbounded
   Float_Random.Reset (Gen);
end PragmARC.Data_Structures.Skip_Lists.Unbounded;
