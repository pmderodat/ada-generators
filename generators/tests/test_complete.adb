with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Support; use Support;

--  Test complete iterations on a generator that yields a specific number of
--  times.

procedure Test_Complete is
   C : Counter_Finite :=
     (Int_Generators.Generator with
      Last => Natural'Value (Ada.Command_Line.Argument (1)));
begin
   for I of Int_Generators.Iterable (C) loop
      Put_Line ("Iteration" & Integer'Image (I));
   end loop;
end Test_Complete;
