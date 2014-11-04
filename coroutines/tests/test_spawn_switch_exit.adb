with Coroutines; use Coroutines;
with Support; use Support;

--  Test spawning a coroutine, switching to it and then letting it stop

procedure Test_Spawn_Switch_Exit is
   C : constant Coroutine := Create (new Null_Delegate);
begin
   C.Spawn;
   C.Switch;
end Test_Spawn_Switch_Exit;
