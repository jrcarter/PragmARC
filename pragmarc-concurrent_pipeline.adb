-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Sep 15     J. Carter          V1.0--Initial release
--
with PragmARC.Holders;

package body PragmARC.Concurrent_Pipeline is
   package body Transformers is
      task Transformer is
         entry Put (Item : in Input);
      end Transformer;

      procedure Put (Item : in Input) is
         -- Empty
      begin -- Put
         Transformer.Put (Item => Item);
      end Put;

      task body Transformer is
         package Holder is new Holders (Element => Input);

         Value : Holder.Handle;
      begin -- Transformer
         Forever : loop
            select
               accept Put (Item : in Input) do
                  Value.Put (Item => Item);
               end Put;

               Handle_Error : begin
                  Put (Item => Transform (Value.Get) );
               exception -- Handle_Error
               when others =>
                  null;
               end Handle_Error;
            or
               terminate;
            end select;
         end loop Forever;
      end Transformer;
   end Transformers;

   package body Sinks is
      task Processor is
         entry Put (Item : in Input);
      end Processor;

      procedure Put (Item : in Input) is
         -- Empty
      begin -- Put
         Processor.Put (Item => Item);
      end Put;

      task body Processor is
         package Holder is new Holders (Element => Input);

         Value : Holder.Handle;
      begin -- Processor
         Forever : loop
            select
               accept Put (Item : in Input) do
                  Value.Put (Item => Item);
               end Put;

               Handle_Error : begin
                  Process (Item => Value.Get);
               exception -- Handle_Error
               when others =>
                  null;
               end Handle_Error;
            or
               terminate;
            end select;
         end loop Forever;
      end Processor;
   end Sinks;
end PragmARC.Concurrent_Pipeline;
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
