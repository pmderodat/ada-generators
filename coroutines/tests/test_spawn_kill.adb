with Support; use Support;

--  Test spawning a coroutine without switching to it, then killing it

procedure Test_Spawn_Kill is
   C : Null_Coroutine;
begin
   C.Spawn;
   C.Kill;
end Test_Spawn_Kill;
