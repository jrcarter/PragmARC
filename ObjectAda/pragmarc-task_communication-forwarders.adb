-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Jun 01     J. Carter          V1.3--Changed comment for empty declarative part
-- 2002 Oct 01     J. Carter          V1.2--Use mode out to allow scalars
-- 2001 Dec 01     J. Carter          V1.1--Added Desired_Priority
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Task_Communication.Forwarders is
   task Handler with Priority => Desired_Priority is
      entry Forward (Item : in Element);
   end Handler;

   procedure Forward (Item : in Element) is
      -- Empty
   begin -- Forward
      Handler.Forward (Item => Item);
   end Forward;

   task body Handler is
      Store : Element;
   begin -- Handler
      Forever : loop
         select
            accept Forward (Item : in Element) do
               Store := Item;
            end Forward;
         or
            terminate;
         end select;

         Put (Item => Store);
      end loop Forever;
   end Handler;
end PragmARC.Task_Communication.Forwarders;
