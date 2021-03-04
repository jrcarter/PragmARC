-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Sep 15     J. Carter          V1.0--Initial release
--
with Ada.Containers.Indefinite_Holders;

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
         package Holder is new Ada.Containers.Indefinite_Holders (Element_Type => Input);

         Value : Holder.Holder;
      begin -- Transformer
         Forever : loop
            select
               accept Put (Item : in Input) do
                  Value.Replace_Element (New_Item => Item);
               end Put;

               Handle_Error : begin
                  Put (Item => Transform (Value.Element) );
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
         package Holder is new Ada.Containers.Indefinite_Holders (Element_Type => Input);

         Value : Holder.Holder;
      begin -- Processor
         Forever : loop
            select
               accept Put (Item : in Input) do
                  Value.Replace_Element (New_Item => Item);
               end Put;

               Handle_Error : begin
                  Process (Item => Value.Element);
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
