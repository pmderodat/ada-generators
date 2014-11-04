with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines; use Coroutines;
with Support; use Support;

--  Test that killing a dead coroutine raises a Coroutine_Error

procedure Test_Kill_Twice is
   C : constant Coroutine := Create (new Null_Delegate);
begin
   C.Kill;
exception
   when Exc : Coroutine_Error =>
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
end Test_Kill_Twice;
