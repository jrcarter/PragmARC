-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V2.1--Added license text, corrected Skip_Line, and set EOL per file
-- 2016 Mar 01     J. Carter          V2.0--Use Sequential_IO so no extra EOLs when a file is closed
-- 2016 Feb 15     J. Carter          V1.0--Initial version
--
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;

package body PragmARC.Text_IO is
   DOS_Windows_String : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   Mac_String         : constant String := Ada.Characters.Latin_1.CR & "";
   Unix_String        : constant String := Ada.Characters.Latin_1.LF & "";

   procedure Create (File : in out File_Handle;
                     Name : in     String                 := "";
                     Mode : in     Character_IO.File_Mode := Out_File;
                     Form : in     String                 := "";
                     EOL  : in     EOL_ID                 := DOS_Windows_EOL)
   is
      -- Empty
   begin -- Create
      Character_IO.Create (File => File.File, Name => Name, Mode => Mode, Form => Form);
      File.Empty := True;
      File.EOL := EOL;
   end Create;

   procedure Open (File : in out File_Handle;
                   Name : in     String;
                   Mode : in     Character_IO.File_Mode := In_File;
                   Form : in     String                 := "";
                   EOL  : in     EOL_ID                 := DOS_Windows_EOL)
   is
      -- Empty
   begin -- Open
      Character_IO.Open (File => File.File, Name => Name, Mode => Mode, Form => Form);
      File.Empty := True;
      File.EOL := EOL;
   end Open;

   procedure Close (File : in out File_Handle) is
      -- Empty
   begin -- Close
      Character_IO.Close (File => File.File);
   end Close;

   function Is_Open (File : in File_Handle) return Boolean is
      (Character_IO.Is_Open (File.File) );

   function Mode (File : in File_Handle) return Character_IO.File_Mode is
      (Character_IO.Mode (File.File) );

   procedure New_Line (File : in out File_Handle; Spacing : in Positive := 1) is
      EOL : constant String := (case File.EOL is
                                when DOS_Windows_EOL => DOS_Windows_String,
                                when Mac_EOL         => Mac_String,
                                when Unix_EOL        => Unix_String);
   begin -- New_Line
      All_Lines : for I in 1 .. Spacing loop
         All_Characters : for C of EOL loop
            Character_IO.Write (File => File.File, Item => C);
         end loop All_Characters;
      end loop All_Lines;
   end New_Line;

   function Get_C (File : in out File_Handle) return Character with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Gets the next Character from File, including EOL Characters

   procedure Put_Back_C (File : in out File_Handle; Item : in Character) with
      Pre => File.Empty or else raise Program_Error with "Put_Back_C: Buffer not empty";
   -- Makes Item the Character that Get_C will return next

   procedure Skip_Line (File : in out File_Handle; Spacing : in Positive := 1) is
      Char1 : Character;
      Char2 : Character;
      EOF   : Boolean := True; -- Indicates if End_Error should be reraised
   begin -- Skip_Line
      All_Lines : for I in 1 .. Spacing loop
         EOF := I < Spacing;

         Find_EOL : loop
            Char1 := Get_C (File);

            exit Find_EOL when Char1 = Ada.Characters.Latin_1.LF;

            if Char1 = Ada.Characters.Latin_1.CR then
               Char2 := Get_C (File);

               if Char2 /= Ada.Characters.Latin_1.LF then
                  Put_Back_C (File => File, Item => Char2);
               end if;

               exit Find_EOL;
            end if;
         end loop Find_EOL;
      end loop All_Lines;
   exception -- Skip_Line
   when Ada.IO_Exceptions.End_Error =>
      if EOF then
         raise;
      end if;
      -- Otherwise we have a final line without a line terminator, or with a Mac line terminator, and we've skipped that line
   end Skip_Line;

   function End_Of_Line (File : in out File_Handle) return Boolean is
      Char : constant Character := Get_C (File);
   begin -- End_Of_Line
      Put_Back_C (File => File, Item => Char);

      return Char = Ada.Characters.Latin_1.CR or Char = Ada.Characters.Latin_1.LF;
   end End_Of_Line;

   function End_Of_File (File : in File_Handle) return Boolean is
      (Character_IO.End_Of_File (File => File.File) );

   procedure Get (File : in out File_Handle; Item : out Character) is
      Char : Character;
   begin -- Get
      Find_Item : loop
         Item := Get_C (File);

         exit Find_Item when Item /= Ada.Characters.Latin_1.CR and Item /= Ada.Characters.Latin_1.LF;

         if Item = Ada.Characters.Latin_1.CR then -- Mac or DOS/Windows EOL
            Char := Get_C (File); -- Check for DOS/Windows EOL

            if Char /= Ada.Characters.Latin_1.LF then
               Put_Back_C (File => File, Item => Char);
            end if;
         end if;
      end loop Find_Item;
   end Get;

   procedure Put (File : in out File_Handle; Item : in Character) is
      -- Empty
   begin -- Put
      Character_IO.Write (File => File.File, Item => Item);
   end Put;

   procedure Get (File : in out File_Handle; Item : out String) is
      -- Empty
   begin -- Get
      Get_All : for I in Item'Range loop
         Get (File => File, Item => Item (I) ); -- Not Get_C, because that will include EOLs
      end loop Get_All;
   end Get;

   procedure Put (File : in out File_Handle; Item : in String) is
      -- Empty
   begin -- Put
      All_Characters : for C of Item loop
         Character_IO.Write (File => File.File, Item => C);
      end loop All_Characters;
   end Put;

   function Get_Line (File : in out File_Handle) return String is
      Line : String (1 .. 1000);
      Last : Natural;
   begin -- Get_Line
      Get_Line (File => File, Item => Line, Last => Last);

      if Last < Line'Last then
         return Line (Line'First .. Last);
      end if;

      return Line & File.Get_Line;
   end Get_Line;

   procedure Get_Line (File : in out File_Handle; Item : out String; Last : out Natural) is
      -- Empty
   begin -- Get_Line
      Last := Item'First - 1;

      Get_Characters : for I in Item'Range loop
         if End_Of_Line (File) then
            Skip_Line (File => File);

            return;
         end if;

         Item (I) := Get_C (File);
         Last := I;
      end loop Get_Characters;
   exception -- Get_Line
   when Ada.IO_Exceptions.End_Error =>
      if Last < Item'First then
         raise;
      end if; -- Otherwise we have a final line without a line terminator, and that line is in Item (Item'First .. Last)
   end Get_Line;

   procedure Put_Line (File : in out File_Handle; Item : in String) is
      -- Empty
   begin -- Put_Line
      Put (File => File, Item => Item);
      New_Line (File => File);
   end Put_Line;

   function Get_C (File : in out File_Handle) return Character is
      Result : Character;
   begin -- Get_C
      if File.Empty then
         Character_IO.Read (File => File.File, Item => Result);
      else
         Result := File.Buffer;
         File.Empty := True;
      end if;

      return Result;
   end Get_C;

   procedure Put_Back_C (File : in out File_Handle; Item : in Character) is
      -- Empty
   begin -- Put_Back_C
      File.Buffer := Item;
      File.Empty := False;
   end Put_Back_C;
end PragmARC.Text_IO;
