-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Compare two Strings in title order, ignoring an initial article
--
-- History:
-- 2022 Aug 01     J. Carter          V1.0--Initial version
--
with Ada.Characters.Handling;

package body PragmARC.Title_Comparisons is
   English_Article : Article_List;

   function English return Article_List is (English_Article);

   function Munged (Item : in String; Article : in Article_List) return String;
   -- Moves any initial Article in Item to the end

   function Munged (Item : in String; Article : in Article_List) return String is
      S : constant String := Ada.Characters.Handling.To_Basic (Ada.Characters.Handling.To_Lower (Item) );
   begin -- Munged
      Check : for I in 1 .. Article.Last_Index loop
         Extract : declare
            A : constant String := Ada.Characters.Handling.To_Basic (Ada.Characters.Handling.To_Lower (Article.Element (I) ) );
         begin -- Extract
            if S'Length > A'Length + 1 and then S (S'First .. S'First + A'Length) = A & ' ' then
               return S (S'First + A'Length + 1 .. S'Last) & ", " & A;
            end if;
         end Extract;
      end loop Check;

      return S;
   end Munged;

   function Less (Left : in String; Right : in String; Article : in Article_List := English) return Boolean is
      (Munged (Left, Article) < Munged (Right, Article) );

   function Greater (Left : in String; Right : in String; Article : in Article_List := English) return Boolean is
      (Munged (Left, Article) > Munged (Right, Article) );
begin -- PragmARC.Title_Comparisons
   English_Article.Append (New_Item => "a");
   English_Article.Append (New_Item => "an");
   English_Article.Append (New_Item => "the");
end PragmARC.Title_Comparisons;
