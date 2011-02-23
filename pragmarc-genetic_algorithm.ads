-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- A generic framework for genetic programming.
--
-- History:
-- 2006 May 01     J. Carter          V1.0--Initial version
--
generic -- PragmARC.Genetic_Algorithm
   type Gene is private;
   
   with function "=" (Left : Gene; Right : Gene) return Boolean is <>;
   
   with function Random return Gene;
   -- Returns a random Gene. Used to initialize the population.
   
   with function Fitness (Individual : Gene) return Float;
   -- Returns the fitness of Individual. Larger values indicate greater fitness.
   
   with function Mate (Left : Gene; Right : Gene) return Gene;
   -- Creates a new Gene by mating Left and Right.
   -- This is often called "crossover".
   
   with procedure Mutate (Individual : in out Gene);
   -- Makes a small change in Individual.
procedure PragmARC.Genetic_Algorithm (Population_Size           : in     Positive :=   100;
                                      Max_Generations           : in     Positive := 1_000;
                                      Num_No_Change_Generations : in     Positive :=    10;
                                      Mutation_Probability      : in     Float    :=     0.1;
                                      Num_Elite_Saved           : in     Natural  :=    10;
                                      Best                      :    out Gene;
                                      Fit                       :    out Float);
-- Evolves a population of Population_Size individuals for at most Max_Generations generations.
-- If the best individual remains unchanged for Num_No_Change_Generations, the procedure may return after fewer generations.
-- Mutation_Probability is the probability that an individual will be mutated. Values < 0.0 are treated the same as 0.0;
-- values > 1.0 are treated the same as 1.0.
-- Num_Elite_Saved is the number of the most fit individuals that survive from one generation to the next.
-- Not surprisingly, Constraint_Error is raised if Num_Elite_Saved > Population_Size.
-- Upon return, Best contains the most fit individual in the final generation and Fit is its fitness.
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
