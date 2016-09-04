-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Simplifies the creation of concurrent pipelines, in which a set of operations
-- are performed concurrently, transforming an input into an ultimate output
--
-- History:
-- 2016 Sep 15     J. Carter          V1.0--Initial release
--
package PragmARC.Concurrent_Pipeline is
   generic -- Transformers
      type Input  (<>) is private;
      type Output (<>) is private;

      with function Transform (Value : Input) return Output;
      with procedure Put (Item : in Output);
   package Transformers is
      procedure Put (Item : in Input);
      -- Blocks until the task doing the transforming is available
   end Transformers;

   generic -- Sinks
      type Input (<>) is private;

      with procedure Process (Item : in Input);
   package Sinks is
      procedure Put (Item : in Input);
      -- Blocks until the task doing the processing is available
   end Sinks;

   -- A package P that is an instantiation of one of these packages contains an internal task that waits until P.Put is called
   -- These tasks terminate via a terminate alternative
   -- If P is an instantiation of Tranformers, the task passes the value from Put to Transform, and passes the result of that
   -- to its generic formal Put
   -- If P is an instantiation of Sinks, the task passes the value from Put to Process
   -- Pipelines are built up in reverse: 1st the Sink, which provides the Put for the ultimate Transformer, which provides the Put
   -- for the penultimate Transformer, which provides the Put ...
   -- If Transform or Process propagates an exception, the processing for that input is silently ended and the task becomes
   -- available for another input

   -- An example of use is

   --  with Ada.Text_IO;
   --  with PragmARC.Concurrent_Pipeline;
   --
   --  procedure Test_CP is
   --     procedure Put_Line (Item : in Integer);
   --     -- Does a Put_Line to Current_Output with Integer'Image (Item)
   --
   --     package Sink is new PragmARC.Concurrent_Pipeline.Sinks (Input => Integer, Process => Put_Line);
   --
   --     function Length (Item : in String) return Integer;
   --     -- Returns Item'Length
   --
   --     package To_Length is new PragmARC.Concurrent_Pipeline.Transformers
   --        (Input => String, Output => Integer, Transform => Length, Put => Sink.Put);
   --
   --     type Int is range -2 ** 15 + 1 .. 2 ** 15 - 1;
   --
   --     function Image (Item : in Int) return String;
   --     -- Returns Int'Image (Item)
   --
   --     package To_Image is new PragmARC.Concurrent_Pipeline.Transformers
   --        (Input => Int, Output => String, Transform => Image, Put => To_Length.Put);
   --
   --     -- This is a pipeline that takes an Int and calls Put_Line on the Length of its Image
   --     -- The Length for the 1st value can be processed by Put_Line while the Image of the 2nd is having its Length found
   --     -- and the 3rd is having its Image made
   --
   --     procedure Put_Line (Item : in Integer) is
   --        -- Empty
   --     begin -- Put_Line
   --        Ada.Text_IO.Put_Line (Item => Integer'Image (Item) );
   --     end Put_Line;
   --
   --     function Length (Item : in String) return Integer is
   --        -- Empty
   --     begin -- Length
   --        return Item'Length;
   --     end Length;
   --
   --     function Image (Item : in Int) return String is
   --        -- Empty
   --     begin -- Image
   --        return Int'Image (Item);
   --     end Image;
   --  begin -- Test_CP
   --     To_Image.Put (Item => Int'First);
   --
   --     Near_Zero : for I in Int range -10 .. 10 loop
   --        To_Image.Put (Item => I);
   --     end loop Near_Zero;
   --
   --     To_Image.Put (Item => Int'Last);
   --  end Test_CP;

   -- When run, this produces

   --  $ ./test_cp
   --   6
   --   3
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   2
   --   3
   --   6

   -- What does this gain us? In this example, nothing, really, except to show how this package is used
   -- However, when the transformations are lengthy and many inputs may be available before one is completely processed,
   -- such a construct allows several inputs to be processed concurrently
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
