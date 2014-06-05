with Ada.Text_IO; use Ada.Text_IO;

with Generators;

procedure Example_Generator is

   package Int_Generators is new Generators (Integer);

   type Counter is new Int_Generators.Generator with record
      Start : Integer;
   end record;

   --------------
   -- Generate --
   --------------

   overriding
   procedure Generate (C : in out Counter) is
      I : Integer := C.Start;
   begin
      loop
         C.Yield (I);
         I := I + 1;
      end loop;
   end Generate;

   --  Iterate over integers starting at 1. Stop at 10.

   C : Counter :=
     (Int_Generators.Generator with
      Start => 1);

begin
   loop
      if not C.Has_Next then
         exit;
      end if;

      declare
         I : constant Integer := C.Next;
      begin
         Put_Line (Integer'Image (I));
         exit when I >= 10;
      end;
   end loop;
end Example_Generator;
