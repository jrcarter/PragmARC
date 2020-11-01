-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
-- **************************************************************************
--
-- A Recursive Error Minimization (REM) neural network
-- Easy to use, fast, & robust
-- Need only specify Num_Input_Nodes, Num_Hidden_Nodes, Num_Output_Nodes, & Num_Patterns
-- If Num_Patterns is not easily determined, set it & R to 1
-- Set Num_Patterns to 1 to use a previously trained network
-- Default values for all other parameters should be satisfactory
--
-- History:
-- 2020 Nov 01     J. Carter          V2.0--Initial Ada-12 version
----------------------------------------------------------------------------
-- 2016 Mar 15     J. Carter          V2.2--Added Random_Weights
-- 2015 Nov 01     J. Carter          V2.1--Removed unused generic parameter
-- 2014 Jul 01     J. Carter          V2.0--Improved interface
-- 2014 Jun 01     J. Carter          V1.1--Added concurrency and GNAT warning
-- 2000 May 01     J. Carter          V1.0--Initial release
--
pragma Assertion_Policy (Check);
pragma Unsuppress (All_Checks);

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

      Input_To_Output_Connections : Boolean := True; -- Must be True if Num_Hidden_Nodes = 0
      Thinning_Active             : Boolean := True;

      -- Network parameters
      Beta : Positive_Real := 0.1; -- Learning rate; 0.1 is usually satisfactory, but values up to 0.5 are often OK

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
      Random_E_Star_Range : Natural_Real := 0.001; -- If > 0, the network will add random noise to E*
      Random_H_Star_Range : Natural_Real := 0.001; -- Ditto for H*

      with procedure Get_Input (Pattern : in Positive; Input : out Node_Set; Desired : out Node_Set);
      -- Gets an input pattern & associated desired output pattern for this pattern #
      -- Called by Prepare_For_Training and Respond
   package REM_NN is
      pragma Assert (Num_Hidden_Nodes > 0 or Input_To_Output_Connections, "REM_NN: Invalid architecture");

      subtype Input_ID  is Positive range 1 .. Num_Input_Nodes;
      subtype Hidden_ID is Positive range 1 .. Num_Hidden_Nodes;
      subtype Output_ID is Positive range 1 .. Num_Output_Nodes;
      subtype Output_Set is Node_Set (Output_ID);

      Deriv_Lim : constant := 1.0E-4;

      type Weight_Group is record
         Weight     : Real    := 0.0;
         Active     : Boolean := True;
         G          : Real    := 2.0 * Ec;
         Delta_W_Rm : Real    := 0.0;
         Deriv_Rm   : Real    := Deriv_Lim;
      end record;

      type IH_Weight_Set is array (Input_ID, Hidden_ID) of Weight_Group;
      type IO_Weight_Set is array (Input_ID, Output_ID) of Weight_Group;
      type Hidden_Bias_Set is array (Hidden_ID) of Weight_Group;
      type HO_Weight_Set is array (Hidden_ID, Output_ID) of Weight_Group;
      type Output_Bias_Set is array (Output_ID) of Weight_Group;

      type Weight_Info is record
         IH_Weight   : IH_Weight_Set;
         IO_Weight   : IO_Weight_Set;
         Hidden_Bias : Hidden_Bias_Set;
         HO_Weight   : HO_Weight_Set;
         Output_Bias : Output_Bias_Set;
      end record;

      procedure Set_Weights (Weight : in Weight_Info);
      -- Sets the network's weights to Weight. Should be called before using Prepare_For_Training, Respond, or Train

      procedure Random_Weights (Max : in Positive_Real := 0.1);
      -- Sets the network's weights to random values in -Max .. Max

      procedure Read (File_Name : in String; Weight : out Weight_Info);
      -- Reads Weight, written by this package, from File_Name

      procedure Read (File_Name : in String);
      -- Equivalent to passing the values obtained by calling Read to Set_Weights

      procedure Prepare_For_Training;
      -- If Num_Patterns > 1 or R > 1.0, this should be called once for a new set of weights before calling Train
      -- Otherwise, this may be skipped

      procedure Get_Weights (Weight : out Weight_Info);
      -- Returns the current network weights in Weight

      procedure Write (File_Name : in String; Weight : in Weight_Info);
      -- Writes Weight to File_Name

      procedure Write (File_Name : in String);
      -- Equivalent to passing the values obtained by calling Get_Weights to Write

      procedure Respond (Pattern : in Positive; Output : out Output_Set; Num_Tasks : in Positive := 1);
      -- Calls Get_Input for this pattern #, and propagates the input through the network to obtain the network's response

      procedure Train (Num_Tasks : in Positive := 1);
      -- Propagates error & derivative backward through the network, & updates the network's weights
   end REM_NN;
end PragmARC.REM_NN_Wrapper;
