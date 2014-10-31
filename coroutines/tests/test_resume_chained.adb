with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines;
with Support; use Support;

--  Test where the completion of a coroutine resumes execution to in a chained
--  scenario.

--  When a coroutine terminates and its creator coroutine is still alive, we
--  expect execution to resume to its creator.

procedure Test_Resume_Chained is

   type Coroutine_A is new Coroutines.Coroutine with null record;
   overriding procedure Run (C : in out Coroutine_A);

   type Coroutine_B is new Coroutines.Coroutine with null record;
   overriding procedure Run (C : in out Coroutine_B);

   ---------
   -- Run --
   ---------

   overriding procedure Run (C : in out Coroutine_A) is
      C_B : Coroutine_B;
   begin
      Put_Line ("Coroutine A: started, about to spawn B");
      C_B.Spawn;
      C_B.Switch;
      Put_Line ("Coroutine A: about to terminate");
   end Run;

   ---------
   -- Run --
   ---------

   overriding procedure Run (C : in out Coroutine_B) is
   begin
      Put_Line ("Coroutine B: started, about to terminate");
   end Run;

   C_A : Coroutine_A;
begin
   Put_Line ("Main: about to spawn A");
   C_A.Spawn;
   C_A.Switch;
   Put_Line ("Main: about to terminate");
end Test_Resume_Chained;
