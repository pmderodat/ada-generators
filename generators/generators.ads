with System.Storage_Elements;

with Coroutines;
with Iterators;

generic
   type T is private;
package Generators is

   type State_Type is
      (Waiting,
       Yielding,
       Returning);

   package T_Iterators is new Iterators (T);

   type Generator is abstract new Coroutines.Coroutine
     and T_Iterators.Iterator
   with private;

   overriding procedure Initialize (I : in out Generator);
   overriding procedure Finalize (I : in out Generator);
   overriding procedure Run (I : in out Generator);

   function Has_Next (I : in out Generator) return Boolean;
   function Next (I : in out Generator) return T;
   procedure Yield (I : in out Generator; Value : T);

   procedure Generate (I : in out Generator) is abstract;

private

   type Generator is abstract new Coroutines.Coroutine
     and T_Iterators.Iterator
   with record
      Caller      : Coroutines.Coroutine_Access;
      State       : State_Type;
      Yield_Value : T;
   end record;

end Generators;
