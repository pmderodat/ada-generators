with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines;
with Support; use Support;

--  Test where the completion of a coroutine resumes execution to

procedure Test_Resume_Simple is
   C : Hello_World_Coroutine :=
     (Coroutines.Coroutine with
      Caller     => Coroutines.Current_Coroutine,
      Iterations => 1);
begin
   C.Spawn;
   Put_Line ("Main: before switch 1");
   C.Switch;
   Put_Line ("Main: before switch 2");
   C.Switch;
   Put_Line ("Main after completion");
end Test_Resume_Simple;
