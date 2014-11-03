with Ada.Text_IO; use Ada.Text_IO;

with Support; use Support;

--  Test stopping an iteration on a generator and then resuming it

procedure Test_Stop_Resume is
   G : Int_Generators.Generator :=
     Int_Generators.Create (new Counter_Infinity);
begin
   for I of G loop
      Put_Line ("Iteration" & Integer'Image (I));
      exit when I = 1;
   end loop;

   for I of G loop
      Put_Line ("Iteration" & Integer'Image (I));
      exit when I = 3;
   end loop;
end Test_Stop_Resume;
