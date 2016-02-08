-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Text I/O to handle text files from multiple platforms
-- Line terminators must be encoded as a sequence of 1 or more characters
-- No column, line, or page counting; no column or page operations
-- End_Of_File works correctly
--
-- History:
-- 2016 Feb 15     J. Carter          V1.0--Initial version
--
with Ada.Characters.Latin_1;
with Ada.Text_IO;

private with Ada.Text_IO.Text_Streams;

package PragmARC.Text_IO is
   DOS_Windows_EOL : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   Mac_Old_EOL     : constant String := Ada.Characters.Latin_1.CR & "";
   Unix_EOL        : constant String := Ada.Characters.Latin_1.LF & "";

   procedure Set_Line_Terminator (EOL : in String);
   -- Sets the line terminator for output to EOL
   -- EOL cannot exceed 5 characters; extra characters will be discarded
   -- The default output EOL is DOS_Windows_EOL

   type File_Handle is tagged limited private;

   -- File modes and forms are interpreted the same as for Ada.Text_IO

   procedure Create (File : in out File_Handle;
                     Name : in     String                := "";
                     Mode : in     Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
                     Form : in     String                := "");
   -- Creates a file named Name with mode Mode and form Form accessible through File
   -- File becomes open
   -- The default Name creates a temporary file
   -- May raise the same exceptions as Ada.Text_IO.Create

   procedure Open (File : in out File_Handle;
                   Name : in     String;
                   Mode : in     Ada.Text_IO.File_Mode := Ada.Text_IO.In_File;
                   Form : in     String                := "");
   -- Opens the file named Name with mode Mode and form Form accessbile through File
   -- File becomes open
   -- May raise the same exceptions as Ada.Text_IO.Open

   procedure Close (File : in out File_Handle);
   -- Closes open file File; File must be open
   -- May raise the same exceptions as Ada.Text_IO.Close
   -- Raises Use_Error if File was obtained through Handle

   function Is_Open (File : File_Handle) return Boolean;
   -- Returns True if File is open; False otherwise

   procedure New_Line (File : in out File_Handle; Spacing : in Positive := 1);
   -- Adds Spacing EOLs to File
   -- File mode must be Out_File or Append_File
   -- May raise the same exceptions as Ada.Text_IO.New_Line

   -- An input EOL is a CR-LF pair, or a single CR not followed by an LF, or a single LF not preceeded by a CR

   procedure Skip_Line (File : in out File_Handle; Spacing : in Positive := 1);
   -- Skips Spacing EOLs in File
   -- File mode must be In_File
   -- May raise the same exceptions as Ada.Text_IO.Skip_Line

   function End_Of_Line (File : File_Handle) return Boolean;
   -- Returns True if the next thing in File is an EOL; False otherwise
   -- File mode must be In_File
   -- May raise the same exceptions as Ada.Text_IO.End_Of_Line

   function End_Of_File (File : File_Handle) return Boolean;
   -- Returns True if the next thing in File is an EOF; False otherwise
   -- File mode must be In_File
   -- May raise the same exceptions as Ada.Text_IO.End_Of_File

   procedure Get (File : in out File_Handle; Item : out Character);
   -- Skips any EOLs in File and then reads a single Character from File into Item
   -- May raise the same exceptions as Ada.Text_IO.Get for Character

   procedure Put (File : in out File_Handle; Item : in Character);
   -- Writes Item to File
   -- May raise the same exceptions as Ada.Text_IO.Put for Character

   procedure Get (File : in out File_Handle; Item : out String);
   -- Gets Item'Length Characters from File into Item
   -- May raise the same exceptions as Ada.Text_IO.Get for String

   procedure Put (File : in out File_Handle; Item : in String);
   -- Puts the Characters in Item to File
   -- May raise the same exceptions as Ada.Text_IO.Put for String

   function Get_Line (File : File_Handle) return String;
   -- Gets Characters from File until an EOL is encountered
   -- Skips the EOL
   -- May raise the same exceptions as function Ada.Text_IO.Get_Line

   procedure Get_Line (File : in out File_Handle; Item : out String; Last : out Natural);
   -- Gets Characters from File until Item is filled or an EOL is encountered
   -- The index of the last position filled in Item is put in Last
   -- If an EOL is encountered, skips the EOL
   -- If End_Of_Line (File), Last will be Item'First - 1
   -- May raise the same exceptions as procedure Ada.Text_IO.Get_Line

   procedure Put_Line (File : in out File_Handle; Item : in String);
   -- Puts Item to File followed by EOL
   -- May raise the same exceptions as Ada.Text_IO.Put_Line
private -- PragmARC.Text_IO
   type Handle_Ptr is access all File_Handle;

   type Rosen_Trick (Ptr : Handle_Ptr) is limited null record;

   type File_Handle is tagged limited record
      Handle : Rosen_Trick (Ptr => File_Handle'Unchecked_Access);
      File   : Ada.Text_IO.File_Type;
      Stream : Ada.Text_IO.Text_Streams.Stream_Access;
      Buffer : Character;
      Empty  : Boolean := True;
   end record;
end PragmARC.Text_IO;
