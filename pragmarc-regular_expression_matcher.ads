-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2019 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Generic regular expression pattern matching
-- A pattern consists of elements
-- Elements can be
--     A literal          Consists of an item            Matches an item if they are equal
--
--     The begin item     Consists of Begin_Set_Item     Makes the rest of the pattern have to match at the start of Source
--
--     The end item       Consists of End_Set_Item       Makes the pattern have to match the last item in S
--ource
--     The wild item      Consists of the item Any_Item  Matches any item
--
--     An escaped item    Consists of an item preceded   Changes the item into a literal
--                        by Escape_Item
--
--     A negated element  Consists of an element pre-    Matches anything but the element
--                        ceded by Not_Item
--
--     A class            Consists of items enclosed by  Matches any one of the items in the class
--                        Start_Class_Item and
--                        Stop_Class_Item; may not con-
--                        tain un-escaped instances of
--                        Any_Item, Not_Item,
--                        Closure_Item, or
--                        Start_Class_Item
--
--     A closure          Consists of an element pre-    Matches zero or more occurrences of the element
--                        ceded by Closure_Item
--
-- History:
-- 2019 Apr 15     J. Carter          V1.2--Sequences indexed by integers; add anchor items to patterns
-- 2001 Feb 01     J. Carter          V1.1--Improve robustness and return length of pattern matched
-- 2000 May 01     J. Carter          V1.0--Initial release
--
private with Ada.Containers.Vectors;

generic -- PragmARC.Regular_Expression_Matcher
   type Item is private;
   type Index is range <>;
   type Item_Set is array (Index range <>) of Item;

   Any_Item         : Item;
   Escape_Item      : Item;
   Not_Item         : Item;
   Closure_Item     : Item;
   Start_Class_Item : Item;
   Stop_Class_Item  : Item;
   Begin_Set_Item   : Item;
   End_Set_Item     : Item;

   with function "=" (Left : Item; Right : Item) return Boolean is <>;
package PragmARC.Regular_Expression_Matcher is
   --pragma Preelaborate;

   Illegal_Pattern : exception; -- Raised when an invalid pattern is processed

   type Processed_Pattern is limited private;

   procedure Process (Pattern : in Item_Set; Processed : in out Processed_Pattern); -- raise Illegal_Pattern
   -- Converts Pattern into an internal format that may be used for repeated calls to Location
   -- Raises Illegal_Pattern if Pattern is illegal
   -- Destroys any pattern currently in Processed, even if Illegal_Pattern was raised

   type Result (Found : Boolean := False) is record
      case Found is
      when False =>
         null;
      when True =>
         Start  : Index;
         Length : Natural;
      end case;
   end record;

   function Location (Pattern : Processed_Pattern; Source : Item_Set) return Result;
   -- Returns Result'(Found => False) if Pattern is not in Source
   -- Returned record has Found => True, Start => location in Source at which Pattern begins,
   -- and Length => # of Items matched by Pattern in Source, otherwise

   function Location (Pattern : Item_Set; Source : Item_Set) return Result; -- raise Illegal_Pattern
   -- Raises Illegal_Pattern if Pattern is not a valid pattern
   -- Applies Process to Pattern, passes the result to Location (Processed_Pattern), and Destroys the processed pattern
   -- Returns the result of calling Location (Processed_Pattern)
private -- PragmARC.Regular_Expression_Matcher
   use Ada;

   type Kind_Id is (Literal, Class, Any, Stop, Beginning, Ending); -- Kind of pattern element

   package Item_Lists is new Containers.Vectors (Index_Type => Positive, Element_Type => Item);

   subtype Class_Info is Item_Lists.Vector; -- Holds the items of a class

   type Expanded_Pattern_Item is record -- A pattern element
      Kind       : Kind_Id := Stop;
      Un_Negated : Boolean := True;
      Closure    : Boolean := False;
      Value      : Item;
      Class_Data : Class_Info;
   end record;

   package Expanded_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Expanded_Pattern_Item);

   type Processed_Pattern is record
      List : Expanded_Lists.Vector;
   end record;
end PragmARC.Regular_Expression_Matcher;
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
