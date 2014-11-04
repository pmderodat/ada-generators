with Coroutines; use Coroutines;

--  Test the finalization of a loop of coroutine references

procedure Test_Reference_Loop is

   type Delegate is new Coroutines.Delegate with record
     Self : Coroutine;
   end record;
   overriding procedure Run (D : in out Delegate);

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Delegate) is
      pragma Unreferenced (D);
   begin
      null;
   end Run;

   D : constant access Delegate := new Delegate;
   C : constant Coroutine := Create (Delegate_Access (D));

begin
   D.Self := C;
end Test_Reference_Loop;
