with PragmARC.Generic_Bounded_Integers;
with Ada.Text_IO; use Ada.Text_IO;

procedure eba is
   package PBI is new PragmARC.Generic_Bounded_Integers(MaxDigits => 2);
   use PBI;
   a1 : Bounded_Integer := To_Bounded_Integer(9);
   -- a2 : Bounded_Integer := To_Bounded_Integer(15);
   -- a3 : Bounded_Integer := To_Bounded_Integer(19);
begin
   -- a3 := a3 + a2 + a1;
   a1 := a1 * To_bounded_Integer(1000000000);

   Put_Line(Image(a1));

end eba;
