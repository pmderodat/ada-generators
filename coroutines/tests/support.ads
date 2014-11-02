with Coroutines;

package Support is

   --  Package providing helpers for test programs

   type Null_Delegate is new Coroutines.Delegate with null record;
   --  Coroutine delegate that does nothing and terminates

   overriding procedure Run (D : in out Null_Delegate);

   type Hello_World_Delegate is new Coroutines.Delegate with record
      Caller     : Coroutines.Coroutine;
      Iterations : Natural;
   end record;
   --  Coroutine delegate that writes "Hello, world!" each time it is switched
   --  to and then switches back to the caller. It stops after performing a
   --  specific number of iterations.

   overriding procedure Run (D : in out Hello_World_Delegate);

end Support;
