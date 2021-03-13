-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A generic framework for genetic programming.
--
-- History:
-- 2021 Mar 15     J. Carter          V2.1--Adapt to changes to quick sort
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2018 Aug 01     J. Carter          V1.3--Cleanup compiler warnings
-- 2017 Jul 15     J. Carter          V1.2--Added tasking
-- 2016 Jun 01     J. Carter          V1.1--Changed comment for empty declarative part and formatting changes
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
---------------------------------------------------------------------------------------------------
-- 2006 May 01     J. Carter          V1.0--Initial version
--
with Ada.Numerics.Float_Random;
with PragmARC.Sorting.Quick;

procedure PragmARC.Genetic_Algorithm (Population_Size           : in     Positive :=   100;
                                      Max_Generations           : in     Positive := 1_000;
                                      Num_No_Change_Generations : in     Positive :=    10;
                                      Mutation_Probability      : in     Float    :=     0.1;
                                      Num_Elite_Saved           : in     Natural  :=    10;
                                      Num_Tasks                 : in     Positive :=     1;
                                      Best                      :    out Gene;
                                      Fit                       :    out Float)
is
   type Member is record
      Individual : Gene;
      Fitness    : Float;
   end record;

   type Population_List is array (Positive range <>) of Member;

   type Best_State is record
      Individual : Gene;
      Count      : Natural := 0;
   end record;

   function "<" (Left : Member; Right : Member) return Boolean is
      (Left.Fitness > Right.Fitness); -- Sort in descending order of fitness.

   package Quick_Sort is new PragmARC.Sorting.Quick (Element => Member, Index => Positive, Sort_Set => Population_List);
   procedure Sort (Set : in out Population_List) renames Quick_Sort.Sort_Sequential;

   procedure Reproduce (List : in out Population_List) is
      New_Pop : Population_List (List'range);

      subtype Index is Positive range List'First .. List'First + List'Length / 2 - 1;
      -- Only the top half of the population get to mate.

      Last_Task : Natural := 0;

      function Task_Num return Positive is
        -- Empty
      begin -- Task_Num
         Last_Task := Last_Task + 1;

         return Last_Task;
      end Task_Num;

      task type Breeder (Num : Positive := Task_Num);
      type Breeder_List is array (1 .. Num_Tasks) of Breeder;

      Genes_Per_Task : constant Positive := (Index'Last - Index'First + 1) / Num_Tasks;

      task body Breeder is
         Prob_Gen : Ada.Numerics.Float_Random.Generator;

         function Choose_Index return Index is -- Pick an Index based on probability of mating.
            function Probability (I : Index) return Float is
               Total_Chances : constant Float := Float ( (Index'Last * (Index'Last + 1) ) / 2);
               Chances       : constant Float := Float (Index'Last - I + 1);
            begin -- Probability
               return Chances / Total_Chances;
            end Probability;

            Prob : constant Float := Ada.Numerics.Float_Random.Random (Prob_Gen);

            Cum_Prob  : Float := 0.0;
         begin -- Choose_Index
            Find : for I in Index loop
               Cum_Prob := Cum_Prob + Probability (I);

               if Prob < Cum_Prob then
                  return I;
               end if;
            end loop Find;

            return Index'Last; -- If Random returns 1.0, which means use the last value.
         end Choose_Index;

         First : constant Positive := (Num - 1) * Genes_Per_Task + 1;

         Last    : Positive := Num * Genes_Per_Task;
         Left    : Index;
         Right   : Index;
         New_Guy : Member;
      begin -- Breeder
         Ada.Numerics.Float_Random.Reset (Gen => Prob_Gen);

         if Num = Num_Tasks then
            Last := Index'Last;
         end if;

         All_Pairs : for I in First .. Last loop
            Left := Choose_Index;

            Get_Right : loop
               Right := Choose_Index;

               exit Get_Right when Right /= Left;
            end loop Get_Right;

            New_Guy.Individual := Mate (List (Left).Individual, List (Right).Individual);

            if Ada.Numerics.Float_Random.Random (Prob_Gen) < Mutation_Probability then
               Mutate (Individual => New_Guy.Individual);
            end if;

            New_Guy.Fitness := Fitness (New_Guy.Individual);
            New_Pop (I) := New_Guy;
         end loop All_Pairs;
      end Breeder;
   begin -- Reproduce
      Breed : declare
         Breeders : Breeder_List;
         pragma Unreferenced (Breeders);
      begin -- Breed
         null;
      end Breed;

      New_Pop (New_Pop'Last - Num_Elite_Saved + 1 .. New_Pop'Last) := List (List'First .. List'First + Num_Elite_Saved - 1);
      Sort (Set => New_Pop);
      List := New_Pop;
   end Reproduce;

   List    : Population_List (1 .. Population_Size);
   Current : Best_State;
begin -- PragmARC.Genetic_Algorithm
   Fill : for I in List'range loop
      List (I).Individual := Random;
      List (I).Fitness    := Fitness (List (I).Individual);
   end loop Fill;

   Sort (Set => List);
   Current := Best_State'(Individual => List (List'First).Individual, Count => 1);

   All_Generations : for I in 1 .. Max_Generations loop
      Reproduce (List => List);

      if Current.Individual = List (List'First).Individual then
         Current.Count := Current.Count + 1;
      else
         Current := Best_State'(Individual => List (List'First).Individual, Count => 1);
      end if;

      exit All_Generations when Current.Count >= Num_No_Change_Generations;
   end loop All_Generations;

   Best := List (List'First).Individual;
   Fit  := List (List'First).Fitness;
end PragmARC.Genetic_Algorithm;
