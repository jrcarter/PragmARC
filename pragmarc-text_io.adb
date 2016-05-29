-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
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

   function Is_Open (File : File_Handle) return Boolean is
      -- Empty
   begin -- Is_Open
      return Character_IO.Is_Open (File.File);
   end Is_Open;

   procedure New_Line (File : in out File_Handle; Spacing : in Positive := 1) is
      function EOL_String return String;
      -- Returns the EOL string for File.EOL

      function EOL_String return String is
         -- Empty
      begin -- EOL_String
         case File.EOL is
         when DOS_Windows_EOL =>
            return DOS_Windows_String;
         when Mac_EOL =>
            return Mac_String;
         when Unix_EOL =>
            return Unix_String;
         end case;
      end EOL_String;

      EOL : constant String := EOL_String;
   begin -- New_Line
      All_Lines : for I in 1 .. Spacing loop
         All_Characters : for J in EOL'Range loop
            Character_IO.Write (File => File.File, Item => EOL (J) );
         end loop All_Characters;
      end loop All_Lines;
   end New_Line;

   function Get_C (File : File_Handle) return Character;
   -- Gets the next Character from File, including EOL Characters

   procedure Put_Back_C (File : in out File_Handle; Item : in Character);
   -- Makes Item the Character that Get_C will return next

   procedure Skip_Line (File : in out File_Handle; Spacing : in Positive := 1) is
      Char1 : Character;
      Char2 : Character;
      EOF   : Boolean := True; -- Indicates if End_Error should be reraised
   begin -- Skip_Line
      All_Lines : for I in 1 .. Spacing loop
         Find_EOL : loop
            Char1 := Get_C (File);
            EOF := I < Spacing;

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

   function End_Of_Line (File : File_Handle) return Boolean is
      Char : constant Character := Get_C (File);
   begin -- End_Of_Line
      Put_Back_C (File => File.Handle.Ptr.all, Item => Char);

      return Char = Ada.Characters.Latin_1.CR or Char = Ada.Characters.Latin_1.LF;
   end End_Of_Line;

   function End_Of_File (File : File_Handle) return Boolean is
      -- Empty
   begin -- End_Of_File
      return Character_IO.End_Of_File (File => File.File);
   end End_Of_File;

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
      All_Characters : for I in Item'Range loop
         Character_IO.Write (File => File.File, Item => Item (I) );
      end loop All_Characters;
   end Put;

   function Get_Line (File : File_Handle) return String is
      Line : String (1 .. 1000);
      Last : Natural;
   begin -- Get_Line
      Get_Line (File => File.Handle.Ptr.all, Item => Line, Last => Last);

      if Last < Line'Last then
         return Line (Line'First .. Last);
      end if;

      return Line & Get_Line (File);
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

   function Get_C (File : File_Handle) return Character is
      F      : File_Handle renames File.Handle.Ptr.all;
      Result : Character;
   begin -- Get_C
      if F.Empty then
         Character_IO.Read (File => F.File, Item => Result);
      else
         Result := F.Buffer;
         F.Empty := True;
      end if;

      return Result;
   end Get_C;

   procedure Put_Back_C (File : in out File_Handle; Item : in Character) is
      -- Empty
   begin -- Put_Back_C
      if not File.Empty then
         raise Program_Error with "Put_Back_C: Buffer not empty";
      end if;

      File.Buffer := Item;
      File.Empty := False;
   end Put_Back_C;
end PragmARC.Text_IO;
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
