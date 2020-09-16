with PragmARC.Generic_Bounded_Integers;
with Ada.Text_IO; use Ada.Text_IO;

procedure eba is
   package PBI is new PragmARC.Generic_Bounded_Integers(Max_Binary_Digits => 63);
   use PBI;
   a1 : Bounded_Integer := Value("-12345678901234567890");
   a2 : Bounded_Integer := To_bounded_Integer(1234567890);
   a3 : Bounded_Integer := Value("12");
   a4 : Bounded_Integer;
begin
   -- a3 := a3 + a2 + a1;
   -- a1 := a1 * To_bounded_Integer(3);
-- 15241578751714678875019052100
   Put_Line(Image(a1, decorated=>True, base => 36));
   Put_Line(Image(a2));
   Put_Line(Image(a3));
   --
   -- a4 := a3 + a1;
   Put_Line(Image(a4));
   Put_Line(Max_Binary_Digits_Info'image);

   -- a4 := a1 * a2 ;
   -- Put_Line(Image(a4));
   --
   -- a4 := a1 / a3 ;
   -- Put_Line(Image(a4));
   --
   -- a4 := a2 * a3 ;
   -- Put_Line(Image(a4));

   null;
end eba;
