with Support; use Support;

--  Test spawning a coroutine without switching to it

procedure Test_Spawn_Exit is
   C : Null_Coroutine;
begin
   C.Spawn;
end Test_Spawn_Exit;
