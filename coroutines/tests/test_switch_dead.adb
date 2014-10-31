with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines;
with Support; use Support;

--  Test switching to a dead coroutine

procedure Test_Switch_Dead is
   C : Null_Coroutine;
begin
   C.Switch;
exception
   when Exc : Coroutines.Coroutine_Error =>
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
end Test_Switch_Dead;
