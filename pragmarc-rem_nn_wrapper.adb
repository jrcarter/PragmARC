-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2016 Jun 01     J. Carter          V2.3--Random_Range moved to PragmARC.Real_Random_Ranges
-- 2016 Jun 01     J. Carter          V2.2--Changed comment for empty declarative part and formatting
-- 2016 Mar 15     J. Carter          V2.1--Added Random_Weights and improved reading and writing weights
-- 2014 Jul 01     J. Carter          V2.0--Improved interface
-- 2014 Jun 15     J. Carter          V1.2--Moved large arrays to heap
-- 2014 Jun 01     J. Carter          V1.1--Added concurrency
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Finalization;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Sequential_IO;
with Ada.Unchecked_Deallocation;

with PragmARC.Real_Random_Ranges;
with PragmARC.Universal_Random;

use Ada;
package body PragmARC.REM_NN_Wrapper is
   package body REM_NN is
      subtype Pattern_ID is Positive range 1 .. Num_Patterns;
      type Desired_Set is array (Pattern_ID) of Output_Set;
      type Count_Value is range 0 .. System.Max_Int;

      -- Basics about nodes:
      -- A node maintains the weights & related values for the connections TO itself
      -- A node also calculates its output value and supplies it to other nodes (those to which it connects)
      -- on demand

      package Input is -- Definition of input nodes
         type Node_Handle is limited private;

         procedure Set_Input (Node : in out Node_Handle; Value : in Real); -- The node accepts its external input value
         function Get_Output (From : Node_Handle) return Real; -- The node provides its output on demand
      private -- Input
         type Node_Handle is record -- An input node just provides its input value as its output
            Output : Real := 0.0;
         end record;
      end Input;

      type Input_Node_Set is array (Input_ID) of Input.Node_Handle;
      type Input_Set_Ptr is access Input_Node_Set;

      type Weight_Set is array (Positive range <>) of Weight_Group;

      package Hidden is -- Definition of hidden nodes
         type Node_Handle is limited private;

         procedure Respond (Node : in out Node_Handle); -- The node collects its input & calculates its output
         function Get_Output (From : Node_Handle) return Real; -- The node provides its output on demand
         procedure Train (Node : in out Node_Handle; ID : in Hidden_ID); -- The node updates weights on connections to it

         -- To use pre-calculated weights, the network has to be able to set weights
         -- To save weights, the network has to be able to obtain weights
         procedure Set_Weight (Node : in out Node_Handle; From : in Input_ID; Weight : in Weight_Group);
         function Get_Weight (Node : Node_Handle; From : Input_ID) return Weight_Group;
         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group);
         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group;
      private -- Hidden
         type Node_Handle is record
            Output : Real := 0.0;
            Deriv  : Real := 0.0;
            Bias   : Weight_Group;
            Weight : Weight_Set (Input_ID); -- Weights from input nodes to this node
         end record;
      end Hidden;

      type Hidden_Node_Set is array (Hidden_ID) of Hidden.Node_Handle;
      type Hidden_Set_Ptr is access Hidden_Node_Set;

      type Star_Group is record
         E_Star : Real := 0.0;
         H_Star : Real := 0.0;
      end record;
      type Star_Set is array (Hidden_ID) of Star_Group;

      package Output is -- Definition of output nodes
         type Node_Handle (Input_To_Output : Boolean) is limited private;

         procedure Respond (Node : in out Node_Handle; Result : out Real);
         -- The node collects its input & calculates its output, which is provided in result
         procedure Train (Node : in out Node_Handle; ID : in Output_ID); -- The node updates weights on connections to it
         function Get_Stars (Node : Node_Handle; From : Hidden_ID) return Star_Group;
         -- The node provides weighted values of E* & H* to hidden nodes on demand

         -- To use pre-calculated weights, the network has to be able to set weights
         -- To save weights, the network has to be able to obtain weights
         procedure Set_Input_Weight (Node : in out Node_Handle; From : in Input_ID; Weight : in Weight_Group);
         function Get_Input_Weight (Node : Node_Handle; From : Input_ID) return Weight_Group;
         procedure Set_Hidden_Weight (Node : in out Node_Handle; From : in Hidden_ID; Weight : in Weight_Group);
         function Get_Hidden_Weight (Node : Node_Handle; From : Hidden_ID) return Weight_Group;
         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group);
         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group;
      private -- Output
         -- An output node has connections from hidden nodes, which have weights to update
         -- The hidden nodes require propagated values of E* & H*
         -- These values must be propagated BEFORE the weights on the connections are updated
         -- Because an output node's TRAIN procedure is called before the hidden node's TRAIN,
         -- the output node stores the weighted values of E* & H* in hidden_star before updating the weights

         type Node_Handle (Input_To_Output : Boolean) is record
            Output        : Real := 0.0;
            Deriv         : Real := 0.0;
            Bias          : Weight_Group;
            Hidden_Weight : Weight_Set (Hidden_ID); -- Weights from hidden nodes to this node
            Hidden_Star   : Star_Set; -- Weighted E* & H* values; see comment block above

            case Input_To_Output is
            when False =>
               null;
            when True =>
               Input_Weight : Weight_Set (Input_ID); -- Weights from input nodes to this node
            end case;
         end record;
      end Output;

      subtype Output_Node_Handle is Output.Node_Handle (Input_To_Output => Input_To_Output_Connections);
      type Output_Node_Set is array (Output_ID) of Output_Node_Handle;
      type Output_Set_Ptr is access Output_Node_Set;

      type Finalizer is new Ada.Finalization.Limited_Controlled with null record;

      overriding procedure Finalize (Object : in out Finalizer);

      Input_Lim  : constant := 300.0;
      H_Star_Lim : constant := 100.0;

      -- Network state information
      Input_Node_Ptr  : Input_Set_Ptr := new Input_Node_Set;
      Input_Node      : Input_Node_Set renames Input_Node_Ptr.all;
      Hidden_Node_Ptr : Hidden_Set_Ptr := new Hidden_Node_Set;
      Hidden_Node     : Hidden_Node_Set renames Hidden_Node_Ptr.all;
      Output_Node_Ptr : Output_Set_Ptr := new Output_Node_Set;
      Output_Node     : Output_Node_Set renames Output_Node_Ptr.all;
      Desired         : Desired_Set := Desired_Set'(others => Output_Set'(others => 0.0) );
      Target          : Output_Set  := Output_Set'(others => 0.0); -- Current D infinity
      Final           : Finalizer;

      Current_Pattern : Positive;

      Cycle_P : Natural_Real := P; -- Values of the parameters which are used, taking into account effect of K_? values
      Cycle_Q : Natural_Real := Q;
      Cycle_S : Natural_Real := S;

      Update_Count : Count_Value := 0;

      package Connection_IO is new Sequential_IO (Element_Type => Weight_Info);

      package Random is new Universal_Random (Supplied_Real => Real);
      package Ranges is new Real_Random_Ranges (Supplied_Real => Real);
      package Real_Math is new Numerics.Generic_Elementary_Functions (Float_Type => Real);

      protected Control is
         procedure Random_Range (Min : in Real; Max : in Real; Result : out Real);
      end Control;

      protected body Control is
         procedure Random_Range (Min : in Real; Max : in Real; Result : out Real) is
            -- Empty
         begin -- Random_Range
            Result := Ranges.Random_Range (Random.Random, Min, Max);
         end Random_Range;
      end Control;

      function Random_Range (Min : Real; Max : Real) return Real is
         Result : Real;
      begin -- Random_Range
         Control.Random_Range (Min => Min, Max => Max, Result => Result);

         return Result;
      end Random_Range;

      -- Transfer: apply the node transfer function to a weighted summed input value
      --           Calculates node output & derivative
      procedure Transfer (Net_Input : in Real; Output : out Real; Deriv : out Real) is
         A : constant Real := Real_Math.Exp (Real'Min (Real'Max (0.5 * Net_Input, -Input_Lim), Input_Lim) );
         B : constant Real := 1.0 / A;
      begin -- Transfer
         Output := (A - B) / (A + B); -- Hyperbolic tangent (tanh)
         Deriv := 2.0 / ( (A + B) ** 2); -- Derivative of tanh
      end Transfer;

      -- New_Rm: Calculate the new value of a Recursive Mean
      function New_Rm (Length : Positive_Real; Old_Value : Real; New_Value : Real) return Real is
         -- Empty
      begin -- New_Rm
         return (1.0 - 1.0 / Length) * Old_Value + (1.0 / Length) * New_Value;
      end New_Rm;

      -- Update_values: Update the values related to a connection
      procedure Update_Values (Sender_Out   : in Real;
                               Receiver_Out : in Real;
                               E_Star       : in Real;
                               H_Star       : in Real;
                               Weight       : in out Weight_Group
                              )
      is
         Delta_W_Lim : constant := 100.0;
         Psi_Lim     : constant := 100.0;
         Denom_Lim   : constant := 1.0E-6;
         Weight_Lim  : constant := 100.0;

         Delta_W : Real;
         Psi     : Real;
         Denom   : Real;
      begin -- Update_Values
         -- Update numerator & denominator of delta W
         Weight.Deriv_Rm := New_Rm (Cycle_P, Weight.Deriv_Rm, (Sender_Out * H_Star) ** 2);
         Weight.Delta_W_Rm := New_Rm (Cycle_Q, Weight.Delta_W_Rm, (Beta / Cycle_P) * Sender_Out * E_Star);

         Delta_W := Real'Min (Real'Max (Weight.Delta_W_Rm / Weight.Deriv_Rm, - Delta_W_Lim), Delta_W_Lim);

         -- Determine if this connection needs to be inactivated or reactivated
         -- Thinning needs the current value of Weight.Weight, so do thinning calculations before applying Delta_W
         if Thinning_Active then
            Psi := Real'Min (Real'Max (2.0 * Sender_Out * Receiver_Out * Weight.Weight, -Psi_Lim), Psi_Lim);
            Denom := (2.0 * Sender_Out * Receiver_Out) ** 2;
            if Denom < Denom_Lim then
               Weight.G := New_Rm (Cycle_S,
                                   Weight.G,
                                   E_Star ** 2 + (Sender_Out * H_Star) ** 2 * 0.5 * Weight.Weight ** 2 *
                                   Real_Math.Exp (2.0 * Sender_Out * Receiver_Out * Weight.Weight) );
            else
               Weight.G := New_Rm (Cycle_S,
                                   Weight.G,
                                   E_Star ** 2 + (Sender_Out * H_Star) ** 2 *
                                   ( (1.0 - (1.0 + Psi) * Real_Math.Exp (-Psi)) / Denom) *
                                   Real_Math.Exp (2.0 * Sender_Out * Receiver_Out * Weight.Weight) );
            end if;

            if Weight.Active then
               Weight.Active := Weight.G > Ec;
            else
               Weight.Active := Weight.G > Ec + Delta_Ec;
            end if;
         end if;

         Weight.Weight := Real'Min (Real'Max (Weight.Weight + Delta_W, -Weight_Lim), Weight_Lim);
      end Update_Values;

      generic  -- ID_Generator
         Num_Tasks : Positive;
      package ID_Generator is
         function Next_ID return Positive;
      end ID_Generator;

      package body ID_Generator is
         Next : Positive := 1;

         function Next_ID return Positive is
            Result : constant Positive := Next;
         begin -- Next_ID
            if Next > Num_Tasks then
               raise Constraint_Error with "Too many tasks";
            end if;

            Next := Next + 1;

            return Result;
         end Next_ID;
      end ID_Generator;

      procedure Set_Weights (Weight : in Weight_Info) is
         -- Empty declarative part
      begin -- Set_Weights
         Set_I : for I in Weight.IH_Weight'Range(1) loop
            I_To_H : for H in Weight.IH_Weight'Range (2) loop
               Hidden.Set_Weight (Node => Hidden_Node (H), From => I, Weight => Weight.IH_Weight (I, H) );
            end loop I_To_H;

            if Input_To_Output_Connections then
               I_To_O : for O in Weight.IO_Weight'Range (2) loop
                  Output.Set_Input_Weight (Node => Output_Node (O), From => I, Weight => Weight.IO_Weight (I, O) );
               end loop I_To_O;
            end if;
         end loop Set_I;

         Set_H : for H in Weight.Hidden_Bias'Range loop
            Hidden.Set_Bias_Weight (Node => Hidden_Node (H), Weight => Weight.Hidden_Bias (H) );

            H_To_O : for O in Weight.HO_Weight'Range (2) loop
               Output.Set_Hidden_Weight (Node => Output_Node (O), From => H, Weight => Weight.HO_Weight (H, O) );
            end loop H_To_O;
         end loop Set_H;

         Set_O : for O in Weight.Output_Bias'Range loop
            Output.Set_Bias_Weight (Node => Output_Node (O), Weight => Weight.Output_Bias (O) );
         end loop Set_O;
      end Set_Weights;

      procedure Random_Weights (Max : in Positive_Real := 0.1) is
         Weight : constant Weight_Info :=
            (IH_Weight   => (others => (others => (Weight => Random_Range (-Max, Max), others => <>) ) ),
             IO_Weight   => (others => (others => (Weight => Random_Range (-Max, Max), others => <>) ) ),
             Hidden_Bias => (others => (Weight => Random_Range (-Max, Max), others => <>) ),
             HO_Weight   => (others => (others => (Weight => Random_Range (-Max, Max), others => <>) ) ),
             Output_Bias => (others => (Weight => Random_Range (-Max, Max), others => <>) ) );
      begin --Random_Weights
         Set_Weights (Weight => Weight);
      end Random_Weights;

      procedure Read (File_Name : in String; Weight : out Weight_Info) is
         File : Connection_IO.File_Type;
      begin -- Read
         Connection_IO.Open (File => File, Mode => Connection_IO.In_File, Name => File_Name);
         Connection_IO.Read (File => File, Item => Weight);
         Connection_IO.Close (File => File);
      end Read;

      procedure Read (File_Name : in String) is
         File   : Connection_IO.File_Type;
         Weight : Weight_Info;
      begin -- Read
         Connection_IO.Open (File => File, Mode => Connection_IO.In_File, Name => File_Name);
         Connection_IO.Read (File => File, Item => Weight);
         Connection_IO.Close (File => File);
         Set_Weights (Weight => Weight);
      end Read;

      procedure Prepare_For_Training is
         -- Empty
      begin -- Prepare_For_Training
         -- Pass each pattern through the network to obtain initial response (D zero)
         All_Patterns : for Pattern in Desired'Range loop
            Respond (Pattern => Pattern, Output => Desired (Pattern) );
         end loop All_Patterns;
      end Prepare_For_Training;

      procedure Get_Weights (Weight : out Weight_Info) is
         -- Empty
      begin -- Get_Weights
         Get_I : for I in Weight.IH_Weight'Range(1) loop
            I_To_H : for H in Weight.IH_Weight'Range (2) loop
               Weight.IH_Weight (I, H) := Hidden.Get_Weight (Node => Hidden_Node (H), From => I);
            end loop I_To_H;

            if Input_To_Output_Connections then
               I_To_O : for O in Weight.IO_Weight'Range (2) loop
                  Weight.IO_Weight (I, O) := Output.Get_Input_Weight (Node => Output_Node (O), From => I);
               end loop I_To_O;
            end if;
         end loop Get_I;

         Get_H : for H in Weight.Hidden_Bias'Range loop
            Weight.Hidden_Bias (H) := Hidden.Get_Bias_Weight (Node => Hidden_Node (H) );

            H_To_O : for O in Weight.HO_Weight'Range (2) loop
               Weight.HO_Weight (H, O) := Output.Get_Hidden_Weight (Node => Output_Node (O), From => H);
            end loop H_To_O;
         end loop Get_H;

         Get_O : for O in Weight.Output_Bias'Range loop
            Weight.Output_Bias (O) := Output.Get_Bias_Weight (Node => Output_Node (O) );
         end loop Get_O;
      end Get_Weights;

      procedure Write (File_Name : in String; Weight : in Weight_Info) is
         File : Connection_IO.File_Type;
      begin -- Write
         Connection_IO.Create (File => File, Name => File_Name);
         Connection_IO.Write (File => File, Item => Weight);
         Connection_IO.Close (File => File);
      end Write;

      procedure Write (File_Name : in String) is
         File   : Connection_IO.File_Type;
         Weight : Weight_Info;
      begin -- Write
         Get_Weights (Weight => Weight);
         Connection_IO.Create (File => File, Name => File_Name);
         Connection_IO.Write (File => File, Item => Weight);
         Connection_IO.Close (File => File);
      end Write;

      procedure Respond (Pattern : in Positive; Output : out Output_Set; Num_Tasks : in Positive := 1) is
         Input_Value : Node_Set (Input_ID);
      begin -- Respond
         Current_Pattern := Pattern;
         Get_Input (Pattern => Pattern, Input => Input_Value, Desired => Target);

         -- Get network response
         -- Send input to input nodes
         Input_Tasks : declare
            Tasks : constant Positive := Integer'Min (Num_Tasks, Input_ID'Last);

            package IDs is new ID_Generator (Num_Tasks => Tasks);

            task type Input_Agent (ID : Positive := IDs.Next_ID);

            task body Input_Agent is
               Start : constant Positive := (ID - 1) * (Input_ID'Last / Tasks) + 1;

               Stop : Positive := Start + Input_ID'Last / Tasks - 1;
            begin -- Input_Agent
               if ID = Tasks then
                  Stop := Input_ID'Last;
               end if;

               All_Input : for Node in Start .. Stop loop
                  Input.Set_Input (Node => Input_Node (Node), Value => Input_Value (Node) );
               end loop All_Input;
            end Input_Agent;

            type Agent_List is array (1 .. Tasks) of Input_Agent;

            Agent : Agent_List;
         begin -- Input_Tasks
            null;
         end Input_Tasks;

         -- For hidden nodes
         if Num_Hidden_Nodes > 0 then
            Hidden_Tasks : declare
               Tasks : constant Positive := Integer'Min (Num_Tasks, Hidden_ID'Last);

               package IDs is new ID_Generator (Num_Tasks => Tasks);

               task type Hidden_Agent (ID : Positive := IDs.Next_ID);

               task body Hidden_Agent is
                  Start : constant Positive := (ID - 1) * (Hidden_ID'Last / Tasks) + 1;

                  Stop  : Positive := Start + Hidden_ID'Last / Tasks - 1;
               begin -- Hidden_Agent
                  if ID = Tasks then
                     Stop := Hidden_ID'Last;
                  end if;

                  All_Hidden : for Node in Start .. Stop loop
                     Hidden.Respond (Node => Hidden_Node (Node) );
                  end loop All_Hidden;
               end Hidden_Agent;

               type Agent_List is array (1 .. Tasks) of Hidden_Agent;

               Agent : Agent_List;
            begin -- Hidden_Tasks
               null;
            end Hidden_Tasks;
         end if;

         -- For output nodes
         Output_Tasks : declare
            Tasks : constant Positive := Integer'Min (Num_Tasks, Output_ID'Last);

            package IDs is new ID_Generator (Num_Tasks => Tasks);

            task type Output_Agent (ID : Positive := IDs.Next_ID);

            task body Output_Agent is
               Start : constant Positive := (ID - 1) * (Output_ID'Last / Tasks) + 1;

               Stop  : Positive := Start + Output_ID'Last / Tasks - 1;
            begin -- Output_Agent
               if ID = Tasks then
                  Stop := Output_ID'Last;
               end if;

               All_Output : for Node in Start .. Stop loop
                  REM_NN.Output.Respond (Node => Output_Node (Node), Result => Output (Node) );
               end loop All_Output;
            end Output_Agent;

            type Agent_List is array (1 .. Tasks) of Output_Agent;

            Agent : Agent_List;
         begin -- Output_Tasks
            null;
         end Output_Tasks;
      end Respond;

      procedure Train (Num_Tasks : in Positive := 1) is
         -- Empty
      begin -- Train
         -- Update global "constants"
         Update_Count := Update_Count + 1;
         Cycle_P := Real'Max (P, Real (Update_Count) * K_P);
         Cycle_Q := Real'Max (Q, Real (Update_Count) * K_Q);
         Cycle_S := Real'Max (S, Real (Update_Count) * K_S);

         Output_Tasks : declare
            Tasks : constant Positive := Integer'Min (Num_Tasks, Output_ID'Last);

            package IDs is new ID_Generator (Num_Tasks => Tasks);

            task type Output_Agent (ID : Positive := IDs.Next_ID);

            task body Output_Agent is
               Start : constant Positive := (ID - 1) * (Output_ID'Last / Tasks) + 1;

               Stop  : Positive := Start + Output_ID'Last / Tasks - 1;
            begin -- Output_Agent
               if ID = Tasks then
                  Stop := Output_ID'Last;
               end if;

               All_Outputs : for Node in Start .. Stop loop
                  Desired (Current_Pattern) (Node) := New_Rm (R, Desired (Current_Pattern) (Node), Target (Node) );
                  Output.Train (Node => Output_Node (Node), ID => Node);
               end loop All_Outputs;
            end Output_Agent;

            type Agent_List is array (1 .. Tasks) of Output_Agent;

            Agent : Agent_List;
         begin -- Output_Tasks
            null;
         end Output_Tasks;

         if Num_Hidden_Nodes > 0 then
            Hidden_Tasks : declare
               Tasks : constant Positive := Integer'Min (Num_Tasks, Hidden_ID'Last);

               package IDs is new ID_Generator (Num_Tasks => Tasks);

               task type Hidden_Agent (ID : Positive := IDs.Next_ID);

               task body Hidden_Agent is
                  Start : constant Positive := (ID - 1) * (Hidden_ID'Last / Tasks) + 1;

                  Stop  : Positive := Start + Hidden_ID'Last / Tasks - 1;
               begin -- Hidden_Agent
                  if ID = Tasks then
                     Stop := Hidden_ID'Last;
                  end if;

                  All_Hidden : for Node in Start .. Stop loop
                     Hidden.Train (Node => Hidden_Node (Node), ID => Node);
                  end loop All_Hidden;
               end Hidden_Agent;

               type Agent_List is array (1 .. Tasks) of Hidden_Agent;

               Agent : Agent_List;
            begin -- Hidden_Tasks
               null;
            end Hidden_Tasks;
         end if;
      end Train;

      package body Input is
         procedure Set_Input (Node : in out Node_Handle; Value : in Real) is
            -- Empty
         begin -- Set_Input
            Node.Output := Value;
         end Set_Input;

         function Get_Output (From : Node_Handle) return Real is
            -- Empty
         begin -- Get_Output
            return From.Output;
         end Get_Output;
      end Input;

      package body Hidden is
         procedure Respond (Node : in out Node_Handle) is
            Net_Input : Real := 0.0;
         begin -- respond
            if Node.Bias.Active then
               Net_Input := Node.Bias.Weight;
            end if;

            Sum_Input : for I_ID in Input_ID loop
               if Node.Weight (I_ID).Active then
                  Net_Input := Net_Input + Input.Get_Output (Input_Node (I_ID) ) * Node.Weight (I_ID).Weight;
               end if;
            end loop Sum_Input;

            Transfer (Net_Input => Net_Input, Output => Node.Output, Deriv => Node.Deriv);
         end Respond;

         function Get_Output (From : Node_Handle) return Real is
            -- Empty
         begin -- Get_Output
            return From.Output;
         end Get_Output;

         procedure Train (Node : in out Node_Handle; ID : in Hidden_ID) is
            Star : Star_Group;
            Prop : Star_Group;
            In_Use : Boolean := False;
         begin -- Train
            -- Sum propagated E* & H* from output nodes
            Sum_Stars : for O_ID in Output_ID loop
               Prop := Output.Get_Stars (Output_Node (O_ID), ID);
               Star := Star_Group'(E_Star => Star.E_Star + Prop.E_Star,
                                   H_Star => Star.H_Star + Prop.H_Star);
            end loop Sum_Stars;

            Star.E_Star := Node.Deriv * Star.E_Star;
            Star.H_Star := Real'Min (Real'Max (Node.Deriv * Star.H_Star, -H_Star_Lim), H_Star_Lim);

            -- Update connections to this node
            Modify : for I_ID in Input_ID loop
               Update_Values (Sender_Out => Input.Get_Output (Input_Node (I_ID) ),
                              Receiver_Out => Node.Output,
                              E_Star => Star.E_Star,
                              H_Star => Star.H_Star,
                              Weight => Node.Weight (I_ID) );
            end loop Modify;

            Update_Values (Sender_Out => 1.0, -- Update bias
                           Receiver_Out => Node.Output,
                           E_Star => Star.E_Star,
                           H_Star => Star.H_Star,
                           Weight => Node.Bias);

            -- Check for inactivity
            Check : for I_ID in Input_ID loop
               In_Use := In_Use or Node.Weight (I_ID).Active;
            end loop Check;

            if not In_Use then -- No active input connections, so turn off bias
               Node.Bias.Active := False;
            end if;
         end Train;

         procedure Set_Weight (Node : in out Node_Handle; From : in Input_ID; Weight : in Weight_Group) is
            -- Empty
         begin -- Set_Weight
            Node.Weight (From) := Weight;
         end Set_Weight;

         function Get_Weight (Node : Node_Handle; From : Input_ID) return Weight_Group is
            -- Empty
         begin -- Get_Weight
            return Node.Weight (From);
         end Get_Weight;

         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group) is
            -- Empty
         begin -- Set_Bias_Weight
            Node.Bias := Weight;
         end Set_Bias_Weight;

         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group is
            -- Empty
         begin -- Get_Bias_Weight
            return Node.Bias;
         end Get_Bias_Weight;
      end Hidden;

      package body Output is
         procedure Respond (Node : in out Node_Handle; Result : out Real) is
            Net_Input : Real := 0.0;
         begin -- Respond
            if Node.Bias.Active then
               Net_Input := Node.Bias.Weight;
            end if;

            if Node.Input_To_Output then
               Sum_Input : for I_ID in Input_ID loop
                  if Node.Input_Weight (I_ID).Active then
                     Net_Input := Net_Input + Input.Get_Output (Input_Node (I_ID) ) * Node.Input_Weight (I_ID).Weight;
                  end if;
               end loop Sum_Input;
            end if;

            Sum_Hidden : for H_ID in Hidden_ID loop
               if Node.Hidden_Weight (H_ID).Active then
                  Net_Input := Net_Input + Hidden.Get_Output (Hidden_Node (H_ID) ) * Node.Hidden_Weight (H_ID).Weight;
               end if;
            end loop Sum_Hidden;

            Transfer (Net_Input => Net_Input, Output => Node.Output, Deriv => Node.Deriv);

            Result := Node.Output;
         end Respond;

         procedure Train (Node : in out Node_Handle; ID : in Output_ID) is
            Star : Star_Group;
         begin -- Train
            -- Calculate E* & H* for this node
            Star.H_Star := Real'Min (Real'Max (Node.Deriv, -H_Star_Lim), H_Star_Lim);
            Star.E_Star := Star.H_Star * (Desired (Current_Pattern) (ID) - Node.Output +
                                          Random_Range (-Random_E_Star_Range, Random_E_Star_Range) );
            Star.H_Star := Star.H_Star + Random_Range (-Random_H_Star_Range, Random_H_Star_Range);

            -- E* & H* have to be propagated back before the weights are updated
            -- This is done by multiplying them by the corresponding weights, & storing the result in Node.Hidden_Star
            -- The values in Node.Hidden_Star are then returned in response to calls to Get_Star
            Adjust_Stars : for H_ID in Hidden_ID loop
               if not Node.Hidden_Weight (H_ID).Active then
                  Node.Hidden_Star (H_ID) := Star_Group'(E_Star => 0.0, H_Star => 0.0);
               else
                  Node.Hidden_Star (H_ID) := Star_Group'(E_Star => Node.Hidden_Weight (H_ID).Weight * Star.E_Star,
                                                         H_Star => Node.Hidden_Weight (H_ID).Weight * Star.H_Star);
               end if;
            end loop Adjust_Stars;

            -- Update all connections to this node
            if Node.Input_To_Output then
               Update_Input : for I_ID in Input_ID loop
                  Update_Values (Sender_Out => Input.Get_Output (Input_Node (I_ID) ),
                                 Receiver_Out => Node.Output,
                                 E_Star => Star.E_Star,
                                 H_Star => Star.H_Star,
                                 Weight => Node.Input_Weight (I_ID) );
               end loop Update_Input;
            end if;

            Update_Hidden : for H_ID in Hidden_ID loop
               Update_Values (Sender_Out => Hidden.Get_Output (Hidden_Node (H_ID) ),
                              Receiver_Out => Node.Output,
                              E_Star => Star.E_Star,
                              H_Star => Star.H_Star,
                              Weight => Node.Hidden_Weight (H_ID) );
            end loop Update_Hidden;

            Update_Values (Sender_Out => 1.0, -- Update bias value
                           Receiver_Out => Node.Output,
                           E_Star => Star.E_Star,
                           H_Star => Star.H_Star,
                           Weight => Node.Bias);
         end Train;

         function Get_Stars (Node : Node_Handle; From : Hidden_ID) return Star_Group is
            -- Empty
         begin -- Get_Stars
            return Node.Hidden_Star (From);
         end Get_Stars;

         procedure Set_Input_Weight (Node : in out Node_Handle; From : in Input_ID; Weight : in Weight_Group) is
            -- Empty
         begin -- Set_Input_Weight
            Node.Input_Weight (From) := Weight;
         end Set_Input_Weight;

         function Get_Input_Weight (Node : Node_Handle; From : Input_ID) return Weight_Group is
            -- Empty
         begin -- Get_Input_Weight
            return Node.Input_Weight (From);
         end Get_Input_Weight;

         procedure Set_Hidden_Weight (Node : in out Node_Handle; From : in Hidden_ID; Weight : in Weight_Group) is
            -- Empty
         begin -- Set_Hidden_Weight
            Node.Hidden_Weight (From) := Weight;
         end Set_Hidden_Weight;

         function Get_Hidden_Weight (Node : Node_Handle; From : Hidden_ID) return Weight_Group is
            -- Empty
         begin -- Get_Hidden_Weight
            return Node.Hidden_Weight (From);
         end Get_Hidden_Weight;

         procedure Set_Bias_Weight (Node : in out Node_Handle; Weight : in Weight_Group) is
            -- Empty
         begin -- Set_Bias_Weight
            Node.Bias := Weight;
         end Set_Bias_Weight;

         function Get_Bias_Weight (Node : Node_Handle) return Weight_Group is
            -- Empty
         begin -- Get_Bias_Weight
            return Node.Bias;
         end Get_Bias_Weight;
      end Output;

      procedure Finalize (Object : in out Finalizer) is
         procedure Free is new Ada.Unchecked_Deallocation (Object => Input_Node_Set,  Name => Input_Set_Ptr);
         procedure Free is new Ada.Unchecked_Deallocation (Object => Hidden_Node_Set, Name => Hidden_Set_Ptr);
         procedure Free is new Ada.Unchecked_Deallocation (Object => OutPut_Node_Set, Name => OutPut_Set_Ptr);
      begin -- Finalize
         Free (Input_Node_Ptr);
         Free (Hidden_Node_Ptr);
         Free (OutPut_Node_Ptr);
      end Finalize;
   begin -- REM_NN
      if Num_Hidden_Nodes <= 0 and then not Input_To_Output_Connections then
         raise Invalid_Architecture;
      end if;

      Random.Randomize;
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
