-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2001 Jun 01     J. Carter          V1.0--Initial release
--
with Ada.Text_IO;

use Ada.Text_IO;
procedure PragmARC.Skip_List_Unbounded.Put (List : in Skip_List) is
   Ptr : Link;
begin -- PragmARC.Skip_List_Unbounded.Put
   Put_Line ("Level:" & Level_Id'Image (List.Level) );
   
   Put_Line ("Header.Forward:");
   
   All_Header_Levels : for Level in List.Header.Forward'range loop
      Put ("  Level:" & Level_Id'Image (Level) & ' ');
      
      if List.Header.Forward (Level) = null then
         Put_Line ("null");
      else
         Put_Line (Image (List.Header.Forward (Level).Value) );
      end if;
   end loop All_Header_Levels;
   
   if List.Last = null then
      Put_Line ("Last: null");
   else
      Put_Line ("Last: " & Image (List.Last.Value) );
   end if;
   
   Ptr := List.Header.Forward (Level_Id'First);
   
   All_Nodes : loop
      exit All_Nodes when Ptr = null;
      
      New_Line;
      Put_Line ("Node:");
      Put_Line ("  Value: " & Image (Ptr.Value) );
      Put_Line ("  Level:" & Level_Id'Image (Ptr.Level) );
      Put_Line ("  Forward:");
      
      All_Node_Levels : for Level in Level_Id'First .. Ptr.Level loop
         Put ("    Level:" & Level_Id'Image (Level) & ' ');
         
         if Ptr.Forward (Level) = null then
            Put_Line ("null");
         else
            Put_Line (Image (Ptr.Forward (Level).Value) );
         end if;
      end loop All_Node_Levels;
      
      Ptr := Ptr.Forward (Level_Id'First);
   end loop All_Nodes;
   
   New_Line;
end PragmARC.Skip_List_Unbounded.Put;
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
