with Coroutines; use Coroutines;
with Support; use Support;

--  Test spawning a coroutine without switching to it

procedure Test_Spawn_Exit is
   C : constant Coroutine := Create (new Null_Delegate);
begin
   C.Spawn;
end Test_Spawn_Exit;
