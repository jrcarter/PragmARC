-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- Generate permutations of a sequence
--
-- History:
-- 2023 Nov 01     J. Carter          V1.0--Initial version
--
package body PragmARC.Permutations is
   procedure Generate (Initial : in Sequence; Process : access procedure (Seq : in Sequence; Stop : in out Boolean) ) is
      procedure Generate (Seq : in out Sequence; Last : in Positive) with
         Pre => Seq'First = Initial'First and Seq'Last = Initial'Last and Last in Seq'Range;
      -- Heap's algorithm for generating PragmARC.Permutations
      -- Generates the PragmARC.Permutations of Seq (1 .. Last), keeping Seq (Last + 1 .. Seq'Last) unchanged, and passes them to Process

      Early_Exit : exception; -- Raised if Process sets Stop to True

      procedure Generate (Seq : in out Sequence; Last : in Positive) is
         procedure Swap (Left : in out Element; Right : in out Element) with
            Post => Left = Right'Old and Right = Left'Old;

         procedure Swap (Left : in out Element; Right : in out Element) is
            Temp : constant Element := Left;
         begin -- Swap
            Left := Right;
            Right := Temp;
         end Swap;

         Stop : Boolean := False;
      begin -- Generate
         if Last = 1 then
            Process (Seq => Seq, Stop => Stop);

            if Stop then
               raise Early_Exit; -- Terminate recursion
            end if;

            return;
         end if;

         Generate (Seq => Seq, Last => Last - 1);

         Sub_Perms : for I in 1 .. Last - 1 loop
            Swap (Left => Seq ( (if Last rem 2 = 0 then I else 1) ), Right => Seq (Last) );
            Generate (Seq => Seq, Last => Last - 1);
         end loop Sub_Perms;
      end Generate;

      Local : Sequence := Initial;
   begin -- Generate
      Generate (Seq => Local, Last => Initial'Last);
   exception -- Generate
   when Early_Exit =>
      null;
   end Generate;

   procedure Generate (Initial : in Sequence; Result : in out Sequence_Lists.Vector) is
      procedure Process (Seq : in Sequence; Stop : in out Boolean);
      -- Appends Seq to Result

      procedure Process (Seq : in Sequence; Stop : in out Boolean) is
         pragma Unreferenced (Stop);
      begin -- Process
         Result.Append (New_Item => Seq);
      end Process;
   begin -- Generate
      Result.Clear;

      Generate (Initial => Initial, Process => Process'Access);
   end Generate;
end PragmARC.Permutations;
