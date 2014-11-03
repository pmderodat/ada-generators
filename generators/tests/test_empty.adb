with Ada.Text_IO; use Ada.Text_IO;

with Support; use Support;

--  Test declaring a generator and then performing no iteration on it

procedure Test_Empty is
   G : Int_Generators.Generator :=
     Int_Generators.Create (new Counter_Infinity);
begin
   Put_Line ("Doing nothing");
end Test_Empty;
