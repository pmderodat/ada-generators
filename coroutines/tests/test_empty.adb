with Coroutines; use Coroutines;
with Support; use Support;

--  Test declaring a coroutine and not spawning it

procedure Test_Empty is
   C : Coroutine := Create (new Null_Delegate);
begin
   null;
end Test_Empty;
