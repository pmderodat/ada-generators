with Coroutines;

package Support is

   --  Package providing helpers for test programs

   type Null_Coroutine is new Coroutines.Coroutine with null record;
   --  Coroutine that does nothing and terminates

   overriding procedure Run (C : in out Null_Coroutine);

   type Hello_World_Coroutine is new Coroutines.Coroutine with record
      Caller     : Coroutines.Coroutine_Access;
      Iterations : Natural;
   end record;
   --  Coroutine that writes "Hello, world!" each time it is switched to and
   --  then switches back to the caller. It stops after performing a specific
   --  number of iterations.

   overriding procedure Run (C : in out Hello_World_Coroutine);

end Support;
