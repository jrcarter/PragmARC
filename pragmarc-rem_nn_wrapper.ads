-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- A Recursive Error Minimization (REM) neural network
-- Easy to use, fast, & robust
-- Need only specify Num_Input_Nodes, Num_Hidden_Nodes, Num_Output_Nodes, & Num_Patterns
-- If Num_Patterns is not easily determined, set it & R to 1
-- Set New_Random_Weights to False & Num_Patterns to 1 to use a previously trained network
-- Set Weight_File_Name as desired
-- Default values for all other parameters should be satisfactory
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with System;
package PragmARC.REM_NN_Wrapper is
   type Real is digits System.Max_Digits; -- Inputs and outputs of the network
   subtype Natural_Real  is Real range 0.0              .. Real'Safe_Last;
   subtype Positive_Real is Real range Real'Model_Small .. Real'Safe_Last;
   
   type Node_Set is array (Positive range <>) of Real;

   generic -- REM_NN
      Num_Input_Nodes  : Positive; -- Network architecture
      Num_Hidden_Nodes : Natural;
      Num_Output_Nodes : Positive;

      Num_Patterns : Positive; -- # of different desired output sets to save & transition

      New_Random_Weights          : Boolean := True; -- False to reuse weights for testing, use, etc.
      Input_To_Output_Connections : Boolean := True; -- Must be True if Num_Hidden_Nodes = 0
      Thinning_Active             : Boolean := True;

      -- Network parameters
      Beta : Positive_Real := 0.1; -- Learning rate; 0.1 has always been satisfactory so far

      -- Recursive means' characteristic lengths:
      P : Positive_Real := 16.0 * Real'Max (Real'Max (Real (Num_Patterns), Real (Num_Input_Nodes) ), Real (Num_Output_Nodes) );
        -- P controls error & denominator of learning rule
      Q : Positive_Real := 0.25 * P; -- Momentum
      R : Positive_Real := Q; -- Transition of desired output
      S : Positive_Real := 4.0 * P; -- G, for thinning

      -- Power-law recursive means: the corresponding recursive mean will change from an exponential to power-law mean when
      -- the parameter * current experience # > corresponding parameter:
      K_P : Natural_Real := 0.0; -- P
      K_Q : Natural_Real := 0.0; -- Q
      K_S : Natural_Real := 0.0; -- S

      -- Thinning parameters:
      Ec       : Natural_Real := 0.001; -- A connection will be inactivated when its G value < Ec
      Delta_Ec : Natural_Real := 0.001; -- A connection will be reactivated when its G value > Ec + Delta_Ec

      -- Random ranges: Random values will be selected from the range -X .. X, where X is one of:
      Random_Weight_Range : Natural_Real := 0.1; -- Initial values for weights
      Random_E_Star_Range : Natural_Real := 0.001; -- If > 0, the network will add random noise to E*
      Random_H_Star_Range : Natural_Real := 0.001; -- Ditto

      -- File name: store, & possibly read, network values from this file
      Weight_File_Name : String := "rem.wgt";

      with procedure Get_Input (Pattern : in Positive; Input : out Node_Set; Desired : out Node_Set);
      -- Gets an input pattern & associated desired output pattern for this pattern #
      -- Called during initialization & by Respond
      -- IMPORTANT:
      -- Since Get_Input is called during the initialization of this package, it must have been elaborated before this package
      -- is instantiated
      -- In practical terms, this means the procedure body of the actual procedure associated with Get_Input must occur before
      -- the instantiation of this package
   package REM_NN is
      subtype Output_Id is Positive range 1 .. Num_Output_Nodes;
      subtype Output_Set is Node_Set (Output_Id);

      procedure Respond (Pattern : in Positive; Output : out Output_Set);
      -- Calls Get_Input for this pattern #, and propagates the input through the network to obtain the network's response

      procedure Train;
      -- Propagates error & derivative backward through the network, & updates the network's weights

      procedure Save_Weights;
      -- Saves the network's values in the files with supplied names

      Invalid_Architecture : exception;
      -- This package can be initialized with Num_Hidden_Nodes = 0 and Input_To_Output_Connections = False
      -- That combination represents an invalid network architecture
      -- The initialization of this package checks for this condition, and raises Invalid_Architecture if it exists
   end REM_NN;
end PragmARC.REM_NN_Wrapper;
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