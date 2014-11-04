with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines; use Coroutines;
with Support; use Support;

--  Test switching to a dead coroutine

procedure Test_Switch_Dead is
   C : constant Coroutine := Create (new Null_Delegate);
begin
   C.Switch;
exception
   when Exc : Coroutine_Error =>
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
end Test_Switch_Dead;
