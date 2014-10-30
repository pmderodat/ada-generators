with Ada.Exceptions;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with System.Storage_Elements;
use type System.Storage_Elements.Storage_Offset;

package Coroutines is

   type Coroutine is abstract tagged limited private;
   type Coroutine_Access is access all Coroutine'Class;

   procedure Spawn
     (C          : in out Coroutine;
      Stack_Size : System.Storage_Elements.Storage_Offset := 2**16);
   --  Spawn a coroutine and initialize it to call Callee. Note that the Switch
   --  primivite has to be invoked so that the execution actually starts. In
   --  order to re-spawn a coroutine, kill it first.

   procedure Switch (C : in out Coroutine);
   --  Switch execution from current coroutine to C.

   function Alive (C : in out Coroutine) return Boolean;
   --  Return whether C is running (thus return False when killed)

   procedure Kill (C : in out Coroutine);
   --  Kill C. An exception is raised in it, then it is cleaned. The main
   --  coroutine cannot be killed, and it is invalid to kill a coroutine
   --  twice in a row.

   procedure Run (C : in out Coroutine) is abstract;
   --  User code to run inside a coroutine

   function Current_Coroutine return Coroutine_Access;
   --  Return a reference to the coroutine that is currently running

   function Main_Coroutine return Coroutine_Access;
   --  Return a reference to the coroutine that was started automatically at
   --  the beginning of the process.

private

   type Coroutine is abstract new Ada.Finalization.Limited_Controlled
     with record
      Data       : System.Address;
      Is_Main    : Boolean;
      Is_Started : Boolean;
      To_Clean   : Boolean;
      Exc        : Ada.Exceptions.Exception_Occurrence;
   end record;

   overriding procedure Initialize (C : in out Coroutine);
   overriding procedure Finalize (C : in out Coroutine);
   --  These two get automatically called thanks to controlled objects

end Coroutines;
