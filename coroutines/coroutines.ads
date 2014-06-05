with Ada.Exceptions;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with System.Storage_Elements;
use type System.Storage_Elements.Storage_Offset;

package Coroutines is

   type Coroutine is tagged limited private;
   type Coroutine_Access is access all Coroutine;

   procedure Initialize (C : in out Coroutine);
   procedure Finalize (C : in out Coroutine);
   --  These two get automatically called thanks to controlled objects

   type Coroutine_Callee is access procedure (C : in out Coroutine);
   --  The kind of procedure a coroutine can start with

   procedure Spawn
     (C          : in out Coroutine;
      Callee     : Coroutine_Callee;
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

   function Current_Coroutine return Coroutine_Access;
   --  Return a reference to the coroutine that is currently running

   function Main_Coroutine return Coroutine_Access;
   --  Return a reference to the coroutine that was started automatically at
   --  the beginning of the process.

private

   type Stack_Type is new System.Storage_Elements.Storage_Array;
   for Stack_Type'Alignment use 8;
   type Stack_Access is access Stack_Type;
   --  Type used to allocate a coroutine execution stack

   procedure Free is new Ada.Unchecked_Deallocation
     (Stack_Type, Stack_Access);

   type Register_Type is
     (RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
      R8,  R9,  R10, R11, R12, R13, R14, R15);

   type Register_Base is array (Register_Type)
     of System.Storage_Elements.Integer_Address;
   --  When context switching from one coroutine to another, this is used to
   --  save/restore the state of a coroutine.

   type Coroutine is new Ada.Finalization.Limited_Controlled with record
      Stack     : Stack_Access;
      Registers : Register_Base;
      Is_Main   : Boolean;
      To_Clean  : Boolean;
      Exc       : Ada.Exceptions.Exception_Occurrence;
   end record;

end Coroutines;
