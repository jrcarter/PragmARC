-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Linear_Equation_Solver is
   function Solve_Linear_Equation (A : Real_Matrix.Matrix; B : Real_Matrix.Vector) return Real_Matrix.Vector is
      My_A : Real_Matrix.Matrix (Num_Rows => A.Num_Rows, Num_Columns => A.Num_Columns) := A;
      My_B : Real_Matrix.Vector (Num_Elements => B.Num_Elements)                       := B;

      type Pivot_Vector is array (1 .. A.Num_Columns) of Positive;

      Pivot  : Pivot_Vector;
      Y      : Real_Matrix.Vector (Num_Elements => A.Num_Columns);
      Result : Real_Matrix.Vector (Num_Elements => A.Num_Columns);
      Sum    : Real;

      Min_Length : constant Positive := Integer'Min (A.Num_Rows, A.Num_Columns);

      Index : Natural := Min_Length;

      Tolerance : constant Real := Real'Epsilon;

      procedure Qr_Factor (A : in out Real_Matrix.Matrix; B : in out Real_Matrix.Vector; Pivot : out Pivot_Vector) is
         My_Pivot : Pivot_Vector;
         Epsilon  : Real := 0.0;
      begin -- Qr_Factor
         Fill_Pivot : for I in My_Pivot'range loop
            My_Pivot (I) := I;
         end loop Fill_Pivot;

         Main : for Count in 0 .. Min_Length - 2 loop
            Work : declare
               Id      : Real_Matrix.Matrix (Num_Rows => A.Num_Rows - Count, Num_Columns => A.Num_Rows - Count);
               A_Col   : Real_Matrix.Vector (Num_Elements => A.Num_Rows - Count);
               W       : Real_Matrix.Vector (Num_Elements => A.Num_Rows - Count);
               Norms   : Real_Matrix.Vector (Num_Elements => A.Num_Columns - Count);
               Index   : Positive;
               Temp    : Positive;
               Elem    : Real;
               Sum_Sqr : Real;
               H_Hat   : Real_Matrix.Matrix (Num_Rows => A.Num_Rows - Count, Num_Columns => A.Num_Rows - Count);
               H       : Real_Matrix.Matrix (Num_Rows => A.Num_Rows, Num_Columns => A.Num_Rows);

               use type Real_Matrix.Matrix;
               use type Real_Matrix.Vector;
            begin -- Work
               Id.Value := (others => (others => 0.0) );

               Fill_Id : for I in Id.Value'range (1) loop
                  Id.Value (I, I) := 1.0;
               end loop Fill_Id;

               W.Value.Value := (others => (others => 0.0) );
               H.Value       := (others => (others => 0.0) );

               Get_Norms : for I in Count + 1 .. A.Num_Columns loop
                  Index := 1;

                  Get_Col : for K in Count + 1 .. A.Num_Rows loop
                     A_Col.Value.Value (Index, 1) := A.Value (K, I);
                     Index := Index + 1;
                  end loop Get_Col;

                  Norms.Value.Value (I - Count, 1) := Real_Matrix.Norm (A_Col);
               end loop Get_Norms;

               Sum_Sqr := Norms * Norms;

               if Count = 0 then
                  Epsilon := Sum_Sqr;
               end if;

               exit Main when abs Epsilon <= Tolerance or else Sum_Sqr < Epsilon * Tolerance;

               Index := 1;

               Largest : for J in Index + 1 .. Norms.Num_Elements loop
                  if Norms.Value.Value (J, 1) > Norms.Value.Value (Index, 1) then
                     Index := J;
                  end if;
               end loop Largest;

               Temp := My_Pivot (Index + Count);
               My_Pivot (Index + Count) := My_Pivot (Count + 1);
               My_Pivot (Count + 1) := Temp;

               Swap : for J in 1 .. A.Num_Rows loop
                  Elem := A.Value (J, Index + Count);
                  A.Value (J, Index + Count) := A.Value (J, Count + 1);
                  A.Value (J, Count + 1) := Elem;
               end loop Swap;

               W.Value.Value (1, 1) := Norms.Value.Value (Index, 1);

               if A.Value (Count + 1, Count + 1) >= 0.0 then
                  W.Value.Value (1, 1) := -W.Value.Value (1, 1);
               end if;

               Fill_Col : for I in 1 .. A_Col.Num_Elements loop
                  A_Col.Value.Value (I, 1) := A.Value (I + Count, Count + 1);
               end loop Fill_Col;

               W := A_Col - W;
               H_Hat := Id - (2.0 / (W * W) ) * (W * Real_Matrix.Transpose (W) );

               Move_Row : for I in 1 .. H_Hat.Num_Rows loop
                  Move_Col : for J in 1 .. H_Hat.Num_Columns loop
                     H.Value (I + Count, J + Count) := H_Hat.Value (I, J);
                  end loop Move_Col;
               end loop Move_Row;

               Fill_New : for I in 1 .. Count loop
                  H.Value (I, I) := 1.0;
               end loop Fill_New;

               A := H * A;
               B := H * B;
            end Work;
         end loop Main;

         Pivot := My_Pivot;
      end Qr_Factor;
   begin -- Solve_Linear_Equation
      if A.Num_Columns /= B.Num_Elements then
         raise Real_Matrix.Size_Mismatch;
      end if;

      Y.Value.Value      := (others => (others => 0.0) );
      Result.Value.Value := (others => (others => 0.0) );

      Qr_Factor (A => My_A, B => My_B, Pivot => Pivot);

      Skip_Indeterminate : loop
         exit Skip_Indeterminate when Index < 1 or else abs My_A.Value (Index, Index) > Tolerance;

         Index := Index - 1;
      end loop Skip_Indeterminate;

      if Index >= 1 then
         Y.Value.Value (Index, 1) := My_B.Value.Value (Index, 1) / My_A.Value (Index, Index);
      end if;

      Determine : for I in reverse 1 .. Index - 1 loop
         Sum := 0.0;

         Add : for J in I + 1 .. Min_Length loop
            Sum := Sum + My_A.Value (I, J) * Y.Value.Value (J, 1);
         end loop Add;

         Y.Value.Value (I, 1) := (My_B.Value.Value (I, 1) - Sum) / My_A.Value (I, I);
      end loop Determine;

      Order : for I in 1 .. Result.Num_Elements loop
         Result.Value.Value (Pivot (I), 1) := Y.Value.Value (I, 1);
      end loop Order;

      return Result;
   end Solve_Linear_Equation;
end PragmARC.Linear_Equation_Solver;
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