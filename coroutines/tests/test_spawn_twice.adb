with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines;
with Support; use Support;

--  Test that spawning twice raises a Coroutine_Error

procedure Test_Spawn_Twice is
   C : Null_Coroutine;
begin
   C.Spawn;
   begin
      C.Spawn;
   exception
      when Exc : Coroutines.Coroutine_Error =>
         Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end;
end Test_Spawn_Twice;
