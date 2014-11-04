with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;
with Support; use Support;

--  Test spawning a coroutine, switching to it back and forth twice, and then
--  kills it

procedure Test_Spawn_Switch_Kill is
   C : constant Coroutine :=
     Create (new Hello_World_Delegate'
               (Caller     => Coroutines.Current_Coroutine,
                Iterations => 3));
begin
   C.Spawn;
   Put_Line ("First switch...");
   C.Switch;
   Put_Line ("Second switch...");
   C.Switch;
   Put_Line ("Done!");
   C.Kill;
end Test_Spawn_Switch_Kill;
