with Ada.Exceptions;
with Ada.Finalization;

with System.Storage_Elements;
use type System.Storage_Elements.Storage_Offset;

pragma Warnings (Off);
private with System.Secondary_Stack;
pragma Warnings (On);

package Coroutines is

   --  This package provides support for creating coroutines.

   --  Coroutines are lightweight user-land cooperative threads. This package
   --  provides a type to hold ref-counted coroutines and an interface to
   --  implement the actual subprogram running under a coroutine.

   type Coroutine is tagged private;
   --  Ref-counted coroutine. Use the Create constructor before using it, or
   --  assign another initialized coroutine to it. Unless explicitely stated,
   --  all primitives expect a coroutine to be initialized and will raise a
   --  Constraint_Error if provided uninitialized ones.

   Null_Coroutine : constant Coroutine;
   --  Uninitialized coroutine

   type Delegate is interface;
   procedure Run (D : in out Delegate) is abstract;
   --  User code to run inside a coroutine. When it completes, the coroutine
   --  becomes dead and the execution resumes to its nearest alive parent.
   --  If it aborts with an exception, the coroutine becomes dead too and
   --  the exception is re-raised in its closest alive parent.

   type Delegate_Access is access all Delegate'Class;

   Coroutine_Error : exception;
   --  Exception raised by coroutine primitives in erroneous cases. Refer to
   --  primitives specifications to learn about these cases.

   function Create (D : Delegate_Access) return Coroutine;
   --  Create and return a new coroutine that will run the D delegate. The
   --  ownership of D is transfered to the coroutine: it will be free'd
   --  when nobody refers to the coroutine anymore. The created coroutine is
   --  associated to the coroutine in which Created is invoked (i.e. this will
   --  be its parent coroutine). In order to actually start the coroutine, use
   --  the Spawn and Switch primitives.

   overriding function "=" (Left, Right : Coroutine) return Boolean;
   --  Return whether Left and Right reference the same coroutine

   function Alive (C : Coroutine) return Boolean;
   --  Return whether C is running (i.e. when it is spawned)

   procedure Spawn
     (C          : Coroutine;
      Stack_Size : System.Storage_Elements.Storage_Offset := 2**16);
   --  Spawn a coroutine and initialize it to call Callee. Note that the Switch
   --  primivite has to be invoked so that the execution actually starts. In
   --  order to re-spawn a coroutine, kill it first.
   --
   --  Spawning a coroutine that is already alive is invalid and raises a
   --  Coroutine_Error.

   procedure Switch (C : Coroutine);
   --  Switch execution from current coroutine to C. Trying to switch to a dead
   --  coroutine or to the coroutine currently running is invalid and raises a
   --  Coroutine_Error.

   procedure Kill (C : Coroutine);
   --  Kill C. An exception is raised in it, then it is cleaned. Trying to
   --  kill the main coroutine or a dead coroutine is invalid and raises a
   --  Coroutine_Error.

   function Current_Coroutine return Coroutine;
   --  Return a reference to the coroutine that is currently running

   function Main_Coroutine return Coroutine;
   --  Return a reference to the coroutine that was started automatically at
   --  the beginning of the process. Trying to kill it is invalid and raises
   --  a Coroutine_Error.

private

   type Coroutine_Internal (D : access Delegate'Class) is
     new Ada.Finalization.Limited_Controlled with record
      Ref_Count  : Natural;
      --  Number of references to this coroutine. Once it reaches 0, the
      --  coroutinen can be free'd.

      Parent     : Coroutine;
      --  Coroutine that created this one. The main coroutine should be the
      --  greatest parent for all coroutines. Used to resume execution after
      --  coroutine completion.

      Data       : System.Address;
      --  Coroutines back-end specific data

      Sec_Stack  : System.Secondary_Stack.SS_Stack_Ptr;
      --  Saved coroutine-specific secondary stack. Allocated (respectively
      --  free'd) when Is_Started is set to True (respectively False).

      Is_Main    : Boolean;
      --  Is this coroutine the main one (i.e. the implicit coroutine started
      --  along with the process).

      Is_Started : Boolean;
      --  True if the coroutine has been spawned *and* switched to at least
      --  once. False in all other cases.

      To_Clean   : Boolean;
      --  Set to True when the coroutine is terminated (because of explicit
      --  abortion, because of an exception or because of regular termination).
      --  False otherwise. It is used by the next coroutine to know when to
      --  clean the previous one. The flag to False is reset once cleaning
      --  is done.

      Exc        : Ada.Exceptions.Exception_Occurrence;
      --  When assigned a non-null exception occurrence, the Switch primitive
      --  must re-raise it when resuming excution.
   end record;
   --  Actual coroutine referenced by Coroutine values

   overriding procedure Initialize (C : in out Coroutine_Internal);
   overriding procedure Finalize (C : in out Coroutine_Internal);

   function Alive (C : in out Coroutine_Internal) return Boolean;
   procedure Spawn
     (C          : in out Coroutine_Internal;
      Stack_Size : System.Storage_Elements.Storage_Offset := 2**16);
   procedure Switch (C : in out Coroutine_Internal);
   procedure Kill (C : in out Coroutine_Internal);
   --  Underlying implementations of the Coroutine primitives

   type Coroutine_Internal_Access is access all Coroutine_Internal;

   type Coroutine is new Ada.Finalization.Controlled with record
      Coroutine : Coroutine_Internal_Access;
   end record;

   overriding procedure Initialize (C : in out Coroutine);
   overriding procedure Adjust (C : in out Coroutine);
   overriding procedure Finalize (C : in out Coroutine);

   Null_Coroutine : constant Coroutine :=
     (Ada.Finalization.Controlled with others => <>);

end Coroutines;
