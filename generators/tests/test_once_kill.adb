with Ada.Text_IO; use Ada.Text_IO;

with Support; use Support;

--  Test performing only one iteration on a generator

procedure Test_Once_Kill is
   G : constant Int_Generators.Generator :=
     Int_Generators.Create (new Counter_Infinity);
begin
   for I of G loop
      Put_Line ("Iteration" & Integer'Image (I));
      exit when I = 0;
   end loop;
end Test_Once_Kill;
