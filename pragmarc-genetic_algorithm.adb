-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A generic framework for genetic programming.
--
-- History:
-- 2021 Jun 01     J. Carter          V2.2--Added Max_Fitness, use vectors, and prevent uninitialized individuals
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
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

procedure PragmARC.Genetic_Algorithm (Population_Size           : in     Positive :=   100;
                                      Max_Generations           : in     Positive := 1_000;
                                      Num_No_Change_Generations : in     Positive :=    10;
                                      Mutation_Probability      : in     Float    :=     0.1;
                                      Num_Elite_Saved           : in     Natural  :=    10;
                                      Num_Tasks                 : in     Positive :=     1;
                                      Max_Fitness               : in     Float    := Float'Last;
                                      Best                      :    out Gene;
                                      Fit                       :    out Float)
is
   type Member is record
      Individual : Gene;
      Fitness    : Float;
   end record;

   package Population_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Member);
   subtype Population_List is Population_Lists.Vector;

   type Best_State is record
      Individual : Gene;
      Count      : Natural := 0;
   end record;

   function "<" (Left : in Member; Right : in Member) return Boolean is
      (Left.Fitness > Right.Fitness); -- Sort in descending order of fitness.

   package Sorting is new Population_Lists.Generic_Sorting;
   procedure Sort (Set : in out Population_List) renames Sorting.Sort;

   procedure Reproduce (List : in out Population_List);
   -- Does one generation of reproduction of the population in List

   procedure Reproduce (List : in out Population_List) is
      New_Pop : Population_List;

      subtype Index is Positive range 1 .. Population_Size / 2;
      -- Only the top half of the population get to mate.

      Last_Task : Natural := 0;

      function Task_Num return Positive;
      -- Increments Last_Task and returns it

      function Task_Num return Positive is
        -- Empty
      begin -- Task_Num
         Last_Task := Last_Task + 1;

         return Last_Task;
      end Task_Num;

      task type Breeder (Num : Positive := Task_Num);
      type Breeder_List is array (1 .. Num_Tasks) of Breeder;

      Num_New      : constant Positive := List.Last_Index - Num_Elite_Saved;
      New_Per_Task : constant Positive := Num_New / Num_Tasks;

      task body Breeder is
         Prob_Gen : Ada.Numerics.Float_Random.Generator;

         function Choose_Index return Index;
         -- Pick an Index based on probability of mating.

         function Choose_Index return Index is
            function Probability (I : Index) return Float;
            -- Calculates the probability that individual I will get to reproduce

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

         First : constant Positive := (Num - 1) * New_Per_Task + 1;

         Last    : Positive := Num * New_Per_Task;
         Left    : Index;
         Right   : Index;
         New_Guy : Member;
      begin -- Breeder
         Ada.Numerics.Float_Random.Reset (Gen => Prob_Gen);

         if Num = Num_Tasks then
            Last := Num_New;
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
            New_Pop.Replace_Element (Index => I, New_Item => New_Guy);
         end loop All_Pairs;
      end Breeder;

      Elite : Positive := 1;
   begin -- Reproduce
      New_Pop.Append (New_Item => (others => <>), Count => Ada.Containers.Count_Type (Population_Size) );

      Breed : declare
         Breeders : Breeder_List;
         pragma Unreferenced (Breeders);
      begin -- Breed
         null;
      end Breed;

      Set_Elite : for I in Population_Size - Num_Elite_Saved + 1 .. Population_Size loop
         New_Pop.Replace_Element (Index => I, New_Item => List.Element (Elite) );
         Elite := Elite + 1;
      end loop Set_Elite;

      Sort (Set => New_Pop);
      List := New_Pop;
   end Reproduce;

   Guy     : Member;
   List    : Population_List;
   Current : Best_State;
begin -- PragmARC.Genetic_Algorithm
   Fill : for I in 1 .. Population_Size loop
      Guy.Individual := Random;
      Guy.Fitness    := Fitness (Guy.Individual);
      List.Append (New_Item => Guy);
   end loop Fill;

   Sort (Set => List);
   Current := Best_State'(Individual => List.Element (1).Individual, Count => 1);

   All_Generations : for I in 1 .. Max_Generations loop
      Reproduce (List => List);

      if Current.Individual = List.Element (1).Individual then
         Current.Count := Current.Count + 1;
      else
         Current := Best_State'(Individual => List.Element (1).Individual, Count => 1);
      end if;

      exit All_Generations when Current.Count >= Num_No_Change_Generations or
                                (Max_Fitness < Float'Last and List.Element (1).Fitness >= Max_Fitness);
   end loop All_Generations;

   Best := List.Element (1).Individual;
   Fit  := List.Element (1).Fitness;
end PragmARC.Genetic_Algorithm;

