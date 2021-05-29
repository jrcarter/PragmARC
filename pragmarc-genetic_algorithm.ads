-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A generic framework for genetic programming.
--
-- History:
-- 2021 Jun 01     J. Carter          V2.2--Added Max_Fitness
-- 2021 May 01     J. Carter          V2.1--Adhere to coding standard
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2017 Jul 15     J. Carter          V1.1--Added tasking
-- 2006 May 01     J. Carter          V1.0--Initial version
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

generic -- PragmARC.Genetic_Algorithm
   type Gene is private;

   with function "=" (Left : in Gene; Right : in Gene) return Boolean is <>;

   with function Random return Gene;
   -- Returns a random Gene. Used to initialize the population.

   with function Fitness (Individual : in Gene) return Float;
   -- Returns the fitness of Individual. Larger values indicate greater fitness.

   with function Mate (Left : in Gene; Right : in Gene) return Gene;
   -- Creates a new Gene by mating Left and Right.
   -- This is often called "crossover".

   with procedure Mutate (Individual : in out Gene);
   -- Makes a small change in Individual.
procedure PragmARC.Genetic_Algorithm (Population_Size           : in     Positive :=   100;
                                      Max_Generations           : in     Positive := 1_000;
                                      Num_No_Change_Generations : in     Positive :=    10;
                                      Mutation_Probability      : in     Float    :=     0.1;
                                      Num_Elite_Saved           : in     Natural  :=    10;
                                      Num_Tasks                 : in     Positive :=     1;
                                      Max_Fitness               : in     Float    := Float'Last;
                                      Best                      :    out Gene;
                                      Fit                       :    out Float)
with
   Pre => Num_Elite_Saved <= Population_Size and Mutation_Probability in 0.0 .. 1.0;
-- Evolves a population of Population_Size individuals for at most Max_Generations generations.
-- If the best individual remains unchanged for Num_No_Change_Generations, the procedure may return after fewer generations.
-- Mutation_Probability is the probability that an individual will be mutated.
-- Num_Elite_Saved is the number of the most fit individuals that survive from one generation to the next.
-- Uses Num_Tasks tasks for generating each generation.
-- Max_Fitness may be used to identify a "perfect" solution, if that value exists, in which case the procedure returns when an
--    individual achieves that fitness; the default means no such check is made
-- Upon return, Best contains the most fit individual in the final generation and Fit is its fitness.
