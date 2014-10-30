with Ada.Text_IO; use Ada.Text_IO;

with Support; use Support;

--  Test performing two iterations on a generator

procedure Test_Twice_Kill is
   C : Counter_Infinity;
begin
   for I of Int_Generators.Iterable (C) loop
      Put_Line ("Iteration" & Integer'Image (I));
      exit when I = 1;
   end loop;
end Test_Twice_Kill;
