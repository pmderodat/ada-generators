with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines; use Coroutines;

--  Test that killing the main coroutine raises a Coroutine_Error

procedure Test_Kill_Main is
begin
   Current_Coroutine.Kill;
exception
   when Exc : Coroutine_Error =>
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
end Test_Kill_Main;
