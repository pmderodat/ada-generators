with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Coroutines;

--  Test that switching to a coroutine that is already running raises a
--  Coroutine_Error.

procedure Test_Switch_Self is
begin
   Coroutines.Current_Coroutine.Switch;
exception
   when Exc : Coroutines.Coroutine_Error =>
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
end Test_Switch_Self;
