with Coroutines; use Coroutines;
with Support; use Support;

--  Test spawning a coroutine without switching to it, then killing it

procedure Test_Spawn_Kill is
   C : constant Coroutine := Create (new Null_Delegate);
begin
   C.Spawn;
   C.Kill;
end Test_Spawn_Kill;
