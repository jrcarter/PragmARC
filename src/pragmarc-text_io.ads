-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Text I/O to handle text files from multiple platforms
-- Line terminators must be encoded as a sequence of 1 or 2 characters
-- No column, line, or page counting; no column or page operations
-- End_Of_File works correctly
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V2.1--Added license text, corrected Skip_Line, and set EOL per file
-- 2016 Mar 01     J. Carter          V2.0--Use Sequential_IO so no extra EOLs when a file is closed
-- 2016 Feb 15     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

with Ada.Sequential_IO;

package PragmARC.Text_IO is
   type EOL_ID is (DOS_Windows_EOL, Mac_EOL, Unix_EOL);
   -- Used to specify what line terminator to use on output
   -- DOS_Windows_EOL = CR-LF
   -- Mac_EOL         = CR
   -- Unix_EOL        = LF

   type File_Handle is tagged limited private;

   package Character_IO is new Ada.Sequential_IO (Element_Type => Character);

   -- File modes and forms are interpreted the same as for Character_IO

   function In_File     return Character_IO.File_Mode renames Character_IO.In_File;
   function Out_File    return Character_IO.File_Mode renames Character_IO.Out_File;
   function Append_File return Character_IO.File_Mode renames Character_IO.Append_File;

   procedure Create (File : in out File_Handle;
                     Name : in     String                 := "";
                     Mode : in     Character_IO.File_Mode := Out_File;
                     Form : in     String                 := "";
                     EOL  : in     EOL_ID                 := DOS_Windows_EOL)
   with
      Pre  => not File.Is_Open,
      Post => File.Is_Open;
   -- Creates a file named Name with mode Mode and form Form accessible through File
   -- File becomes open
   -- The default Name creates a temporary file
   -- EOL indicates what kind of line terminators to use on output; it is ignored if Mode = In_File
   -- May raise the same exceptions as Character_IO.Create

   procedure Open (File : in out File_Handle;
                   Name : in     String;
                   Mode : in     Character_IO.File_Mode := In_File;
                   Form : in     String                 := "";
                   EOL  : in     EOL_ID                 := DOS_Windows_EOL)
   with
      Pre  => not File.Is_Open,
      Post => File.Is_Open;
   -- Opens the file named Name with mode Mode and form Form, making it accessbile through File
   -- File becomes open
   -- EOL indicates what kind of line terminators to use on output; it is ignored if Mode = In_File
   -- May raise the same exceptions as Character_IO.Open

   procedure Close (File : in out File_Handle) with
      Pre  => File.Is_Open,
      Post => not File.Is_Open;
   -- Closes open file File; File must be open
   -- May raise the same exceptions as Character_IO.Close

   function Is_Open (File : in File_Handle) return Boolean;
   -- Returns True if File is open; False otherwise

   function Mode (File : in File_Handle) return Character_IO.File_Mode with
      Pre => File.Is_Open;
   -- Returns the mode with which File was created or opened

   procedure New_Line (File : in out File_Handle; Spacing : in Positive := 1) with
      Pre => File.Is_Open and then File.Mode in Out_File | Append_File;
   -- Adds Spacing EOLs to File

   -- An input EOL is a CR-LF pair, a single CR not followed by an LF, or a single LF not preceeded by a CR

   use type Character_IO.File_Mode;

   procedure Skip_Line (File : in out File_Handle; Spacing : in Positive := 1) with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Skips Spacing EOLs in File

   function End_Of_Line (File : in out File_Handle) return Boolean with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Returns True if the next thing in File is an EOL; False otherwise

   function End_Of_File (File : in File_Handle) return Boolean with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Returns True if the next thing in File is an EOF; False otherwise
   -- May raise the same exceptions as Character_IO.End_Of_File

   procedure Get (File : in out File_Handle; Item : out Character) with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Skips any EOLs in File and then reads a single Character from File into Item
   -- May raise the same exceptions as Character_IO.Read

   procedure Put (File : in out File_Handle; Item : in Character) with
      Pre => File.Is_Open and then File.Mode in Out_File | Append_File;
   -- Writes Item to File
   -- May raise the same exceptions as Character_IO.Write

   procedure Get (File : in out File_Handle; Item : out String) with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Gets Item'Length Characters from File into Item
   -- May raise the same exceptions as Get for Character

   procedure Put (File : in out File_Handle; Item : in String) with
      Pre => File.Is_Open and then File.Mode in Out_File | Append_File;
   -- Puts the Characters in Item to File
   -- May raise the same exceptions as Put for Character

   function Get_Line (File : in out File_Handle) return String with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Gets Characters from File until an EOL is encountered
   -- Skips the EOL
   -- May raise the same exceptions as Get

   procedure Get_Line (File : in out File_Handle; Item : out String; Last : out Natural) with
      Pre => File.Is_Open and then File.Mode = In_File;
   -- Gets Characters from File until Item is filled or an EOL is encountered
   -- The index of the last position filled in Item is put in Last
   -- If an EOL is encountered, skips the EOL
   -- If End_Of_Line (File), Last will be Item'First - 1
   -- May raise the same exceptions as procedure Get

   procedure Put_Line (File : in out File_Handle; Item : in String) with
      Pre => File.Is_Open and then File.Mode in Out_File | Append_File;
   -- Puts Item to File followed by EOL
   -- May raise the same exceptions as Put and New_Line
private -- PragmARC.Text_IO
   type File_Handle is tagged limited record
      File   : Character_IO.File_Type;
      EOL    : EOL_ID := DOS_Windows_EOL;
      Buffer : Character;
      Empty  : Boolean := True;
   end record;
end PragmARC.Text_IO;
