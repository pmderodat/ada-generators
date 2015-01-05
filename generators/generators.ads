with Ada.Finalization;

with Coroutines;

generic
   type T is private;
package Generators is

   --  This package provides support for creating generators

   --  Generators are procedures that return ("yield") multiple results
   --  incrementally. This package provides a type to hold ref-counted
   --  generators and an interface to implement the actual generator
   --  procedure.

   type Generator is tagged private
     with Iterable => (First       => First,
                       Next        => Next,
                       Has_Element => Has_Element,
                       Element     => Element);
   --  Ref-counted generator that yields T values. Use the Create constructor
   --  before using it, or assign another initialized generator to it.
   --  All primitives expect initialized generators and will raise a
   --  Constraint_Error if provided uninitialized ones.

   Null_Generator : constant Generator;

   function Is_Null (G : Generator) return Boolean;

   type Cursor_Type is null record;
   --  Cursor type used in the generator iteration interface. Due to the nature
   --  of generators, cursors do not hold any state: only the generator does.
   --  Hence, iteration on a generator is a one-way process only.

   type Delegate is interface;
   procedure Generate (D : in out Delegate; G : Generator'Class) is abstract;
   --  User code to run as a generator. Inside a generator, the only Generator
   --  primitive that is valid to invoke is Yield. When it completes, all
   --  iterations on it will finish. If it aborts with an exception, the
   --  iteration aborts with an exception.

   type Delegate_Access is access all Delegate'Class;

   Generator_Error : exception;
   --  Exception raised by generator primitives in erroneous cases. Refer to
   --  primitives specifications to learn about these cases.

   function Create (D                  : Delegate_Access;
                    Transfer_Ownership : Boolean := True) return Generator;
   --  Create and return a new generator that will run the D delegate. If
   --  Transfer_Ownership is true, the ownership of D is transfered to the
   --  generator: it will be free'd when nobody refers to the coroutine
   --  anymore; otherwise it is up to the caller to make sure D is free'd while
   --  the coroutine is not running anymore. Creating a generator starts the
   --  generation (i.e. the delegate is started here).

   procedure Yield (G : Generator; Value : T);
   --  Yield a value.  Must be called from the Generate procedure only.

   -------------------------------
   -- Basic iteration interface --
   -------------------------------

   function Has_Next (G : Generator) return Boolean;
   --  Return whether G has a value to yield. Resume the generator to find out
   --  if needed.

   function Next (G : Generator) return T;
   --  Assuming G has a value to yield, return it and go to next iteration

   --------------------------------
   -- Iterable aspect primitives --
   --------------------------------

   function First (G : Generator) return Cursor_Type;
   --  Return a cursor. As stated above, all cursors are identical, they hold
   --  no information.

   function Next (G : Generator; C : Cursor_Type) return Cursor_Type;
   --  Assuming G is not done, resume it to make it yield its next element. As
   --  the generator may stop, one has to check whether it did yield something
   --  before retreiving it with the Element primitive.

   function Has_Element (G : Generator; C : Cursor_Type) return Boolean;
   --  Return whether the last call to Next on G yielded an element

   function Element (G : Generator; C : Cursor_Type) return T;
   --  Assuming Has_Element is true, return the element the last call to Next
   --  on G yielded.

private

   type State_Type is
     (Waiting,
      --  The generator has already yielded or was just created, and is waiting
      --  to be resumed.

      Yielding,
      --  The generator just yielded and is waiting for its caller to get the
      --  value.

      Returning
      --  The generator has not yielded and is done
     );
   --  Describe the execution state of a generator

   type Generator_Internal is
     new Ada.Finalization.Limited_Controlled with record
      Ref_Count   : Natural;
      --  Number of references to this generator. Once it reaches 0, the
      --  coroutine can be free'd.

      Delegate    : Delegate_Access;
      --  User delegate, to be run under Generator_Delegate

      Owns_Delegate : Boolean;
      --  Whether Delegate is owned by this generator. If it's the case, the
      --  Delegate is free'd when the generator is free'd.

      Coroutine   : Coroutines.Coroutine;
      --  Coroutine that runs this generator

      Caller      : Coroutines.Coroutine;
      --  Just before switching to the generator coroutine, set to reference
      --  the coroutine it is supposed to switch back to.

      State       : State_Type;
      --  Generator execution state. Used to synchronize the generator and its
      --  caller.

      Yield_Value : T;
      --  Holds values the generator yields so that the caller can access it
   end record;

   overriding procedure Initialize (G : in out Generator_Internal);
   overriding procedure Finalize (G : in out Generator_Internal);

   type Generator_Internal_Access is access all Generator_Internal;

   type Generator_Delegate is new Coroutines.Delegate with record
      Generator : Generator_Internal_Access;
   end record;
   type Generator_Delegate_Access is access all Generator_Delegate;
   --  Delegate that actually implements the generator's coroutine. This is
   --  what invoke the user delegate.

   overriding procedure Run (D : in out Generator_Delegate);

   type Generator is new Ada.Finalization.Controlled with record
      Generator : Generator_Internal_Access;

      Weak      : Boolean;
      --  Whether finalization should trigger reference counting and garbage
      --  collection.
   end record;

   overriding procedure Initialize (G : in out Generator);
   overriding procedure Adjust (G : in out Generator);
   overriding procedure Finalize (G : in out Generator);

   Null_Generator : constant Generator :=
     (Ada.Finalization.Controlled with Generator => null, Weak => False);

end Generators;
