-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Oct 15     J. Carter          V1.1--Correct an error when opening an existing list with Write_On_Modify True
-- 2014 Oct 01     J. Carter          V1.0--Initial version
--
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Unchecked_Conversion;

package body PragmARC.Persistent_Skip_List_Unbounded is
   package Element_IO is new Ada.Sequential_IO (Element_Type => Element);

   procedure Write (Filename : in String; List : in out Lists.Skip_List);
   -- Writes List to Filename

   function Open_List (Filename : in String; Write_On_Modify : in Boolean := False) return Persistent_Skip_List is
      File : Element_IO.File_Type;
      Item : Element;
   begin -- Open_List
      return Result : Persistent_Skip_List do
         Result.Filename := Ada.Strings.Unbounded.To_Unbounded_String (Filename);
         Result.Write_On_Modify := Write_On_Modify;

         if not Ada.Directories.Exists (Filename) then
            Element_IO.Create (File => File, Name => Filename);
            Element_IO.Close (File => File);
         else
            Element_IO.Open (File => File, Mode => Element_IO.In_File, Name => Filename);

            All_Items : loop
               exit All_Items when Element_IO.End_Of_File (File);

               Element_IO.Read (File => File, Item => Item);
               Result.List.Insert (Item => Item);
            end loop All_Items;

            Element_IO.Close (File => File);
         end if;
      end return;
   exception -- Open_List
   when others =>
      if Element_IO.Is_Open (File) then
         Element_IO.Close (File => File);
      end if;

      raise Invalid_File;
   end Open_List;

   procedure Clear (List : in out Persistent_Skip_List) is
      -- Empty declarative part
   begin -- Clear
      List.List.Clear;

      if List.Write_On_Modify then
         Write (Filename => Ada.Strings.Unbounded.To_String (List.Filename), List => List.List);
      end if;
   end Clear;

   function Search (List : in Persistent_Skip_List; Item : in Element) return Result is
      function Convert is new Ada.Unchecked_Conversion (Source => Lists.Result, Target => Result);
   begin -- Search
      return Convert (List.List.Search (Item) );
   end Search;

   procedure Insert (List : in out Persistent_Skip_List; Item : in Element) is
      -- Empty declarative part
   begin -- Insert
      List.List.Insert (Item => Item);

      if List.Write_On_Modify then
         Write (Filename => Ada.Strings.Unbounded.To_String (List.Filename), List => List.List);
      end if;
   end Insert;

   procedure Delete (List : in out Persistent_Skip_List; Item : in Element) is
      -- Empty declarative part
   begin -- Delete
      List.List.Delete (Item => Item);

      if List.Write_On_Modify then
         Write (Filename => Ada.Strings.Unbounded.To_String (List.Filename), List => List.List);
      end if;
   end Delete;

   function Get_First (List : in Persistent_Skip_List) return Element is
      (List.List.Get_First);

   function Get_Last (List : in Persistent_Skip_List) return Element is
      (List.List.Get_Last);

   function Is_Empty (List : in Persistent_Skip_List) return Boolean is
      (List.List.Is_Empty);

   function Length (List : in Persistent_Skip_List) return Natural is
      (List.List.Length);

   procedure Iterate (List : in out Persistent_Skip_List) is
      procedure Internal is new Lists.Iterate (Action => Action);
   begin -- Iterate
      Internal (List => List.List);
   end Iterate;

   procedure Finalize (Object : in out Persistent_Skip_List) is
      -- Empty declarative part
   begin -- Finalize
      if not Object.Finalized then
         Object.Finalized := True;

         if not Object.Write_On_Modify then
            Write (Filename => Ada.Strings.Unbounded.To_String (Object.Filename), List => Object.List);
         end if;
      end if;
   exception -- Finalize
   when others =>
      null;
   end Finalize;

   procedure Write (Filename : in String; List : in out Lists.Skip_List) is
      File : Element_IO.File_Type;

      procedure Write_One (Item : in Element);
      -- Writes Item to File

      procedure Write_All is new Lists.Iterate (Action => Write_One);

      procedure Write_One (Item : in Element) is
         -- Empty declarative part
      begin -- Write_One
         Element_IO.Write (File => File, Item => Item);
      end Write_One;
   begin -- Write
      Element_IO.Create (File => File, Name => Filename);
      Write_All (List => List);
      Element_IO.Close (File => File);
   end Write;
end PragmARC.Persistent_Skip_List_Unbounded;
