--  Generator that implements the custom iterator interface. Users only need to
--  override the Generate primitive.

with System.Storage_Elements;

with Coroutines;
with Iterators;

generic
   type T is private;
package Generators is

   --  This package provides support for creating generators.

   --  Generators are procedures that return ("yield") multiple results
   --  incrementally. This package provides an abstract tagged type: in order
   --  to create generators, one has to derive in order to provide such a
   --  procedure as a primitive.

   ---------------------
   -- Generator types --
   ---------------------

   type Generator;
   type Generator_Access is access all Generator;

   type Cursor_Type is null record;
   --  Cursor type used in the generator iteration interface. Due to the nature
   --  of generators, cursors do not hold any state: only the generator does.
   --  Hence, iteration on a generator is a one-way process only.

   package T_Iterators is new Iterators (T, Cursor_Type);
   subtype Iterable is T_Iterators.Iterator'Class;

   type Generator is abstract limited new T_Iterators.Iterator with private;
   --  A generator that yields T values

   -------------------------------
   -- Basic iteration interface --
   -------------------------------

   function Has_Next (G : in out Generator) return Boolean;
   --  Return whether G has a value to yield. Resume the generator to find out
   --  if needed.

   function Next (G : in out Generator) return T;
   --  Assuming G has a value to yield, return it and go to next iteration

   --------------------------------
   -- Iterable aspect primitives --
   --------------------------------

   overriding function First (G : in out Generator) return Cursor_Type;
   --  Return a cursor. As stated above, all cursors are identical, they hold
   --  no information.

   overriding function Next (G : in out Generator; C : Cursor_Type)
                             return Cursor_Type;
   --  Assuming G is not done, resume it to make it yield its next element. As
   --  the generator may stop, one has to check whether it did yield something
   --  before retreiving it with the Element primitive.

   overriding function Has_Element (G : in out Generator; C : Cursor_Type)
                                    return Boolean;
   --  Return whether the last call to Next on G yielded an element

   overriding function Element (G : in out Generator; C : Cursor_Type)
                                return T;
   --  Assuming Has_Element is true, return the element the last call to Next
   --  on G yielded.

   procedure Yield (G : in out Generator; Value : T);
   --  Yield a value.  Must be called from the Generate procedure only.

   procedure Generate (G : in out Generator) is abstract;
   --  Primite one has to override in order to implement a generator. This is
   --  a simple procedure that calls Yield multiple times in order to yield
   --  multiple results.

private

   type State_Type is
     (Waiting,
      Yielding,
      Returning);
   --  Describe the execution state of a generator

   type Generator is abstract new Coroutines.Coroutine
     and T_Iterators.Iterator with record
      Caller      : Coroutines.Coroutine_Access;
      --  Identifies the coroutine that spawned this generator. This enables us
      --  to switch back to it when yielding a result.

      State       : State_Type;
      --  Generator execution state. Used to synchronize the generator and its
      --  caller.

      Yield_Value : T;
      --  Holds values the generator yields so that the caller can access it
   end record;

   overriding procedure Initialize (G : in out Generator);
   --  Associate G to its caller and spawn it

   overriding procedure Finalize (G : in out Generator);
   --  Stop the coroutine

   overriding procedure Run (G : in out Generator);
   --  Wrapper implementing a coroutine

end Generators;
