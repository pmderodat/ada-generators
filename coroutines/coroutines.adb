with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with System; use System;
with Ada.Exceptions.Is_Null_Occurrence;

package body Coroutines is

   --------------------
   -- Main coroutine --
   --------------------

   type Main_Coroutine_Type is new Coroutine with null record;

   procedure Run (C : in out Main_Coroutine_Type);
   --  The main coroutine... is never started the usual way: it is the
   --  "regular" coroutine. Thus, this Run method will never be used.

   ---------
   -- Run --
   ---------

   procedure Run (C : in out Main_Coroutine_Type) is
   begin
      --  This is not supposed to be run: see the above comment

      raise Program_Error;
   end Run;

   Main_Coroutine_Obj : aliased Main_Coroutine_Type :=
     (Ada.Finalization.Limited_Controlled with
      Stack     => null,
      Registers => <>,
      Is_Main   => True,
      To_Clean  => False,
      Exc       => <>);

   Abort_Coroutine : exception;
   --  Users should not be able to stop coroutine abortion. Use
   --  Standard'Abort_Signal instead???

   Current_Coroutine_Ptr : Coroutine_Access := Main_Coroutine_Obj'Access;
   pragma Export (C, Current_Coroutine_Ptr, "coroutines__current");

   Previous_Coroutine_Ptr : Coroutine_Access := null;
   pragma Export (C, Previous_Coroutine_Ptr, "coroutines__previous");

   procedure Switch_Helper (C : System.Address);
   pragma Import (C, Switch_Helper, "coroutines__switch_helper");

   function "=" (Left, Right : Coroutine'Class) return Boolean is
     (Left'Address = Right'Address);

   procedure Top_Wrapper (C : in out Coroutine'Class);
   --  Coroutines landing pad: first subprogram executed in a coroutine

   procedure Coroutine_Wrapper (C : in out Coroutine'Class);
   --  Wrapper for coroutines execution: catch exceptions, manage state
   --  finalization.

   procedure Reraise_And_Clean (Exc : in out Exception_Occurrence);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : in out Coroutine) is
   begin
      C.Stack := null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (C : in out Coroutine) is
   begin
      if C /= Main_Coroutine.all and then C.Stack /= null then
         C.Kill;
         Free (C.Stack);
      end if;
   end Finalize;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (C          : in out Coroutine;
      Stack_Size : System.Storage_Elements.Storage_Offset := 2**16)
   is
      use System.Storage_Elements;
      use Interfaces;

      type Stack_Pointer is access all Storage_Element;
      type Word_Access is access all Integer_Address;

      function Convert is new Ada.Unchecked_Conversion
        (Stack_Pointer, Word_Access);

      Word_Size          : constant Storage_Offset :=
        System.Word_Size / System.Storage_Unit;
      Stack_Aligned_Size : constant Storage_Offset :=
        Stack_Size - Stack_Size mod Word_Size;

      Stack_Top : constant Storage_Offset :=
        Stack_Aligned_Size - Word_Size + 1;
      Cursor    : Storage_Offset := Stack_Top;
      --  On x86_64 systems, the stack goes to lower addresses: start pushing
      --  elements at the end of it.

      procedure Push (Value : Integer_Address);

      procedure Push (Value : Integer_Address) is
         Ptr : constant Stack_Pointer := C.Stack (Cursor)'Access;
      begin
         Convert (Ptr).all := Value;
         Cursor := Cursor - Word_Size;
      end Push;

   begin
      if C.Alive then
         raise Program_Error with "Coroutine already running";
      end if;

      C.Is_Main := False;
      C.To_Clean := False;
      Save_Occurrence (C.Exc, Null_Occurrence);

      C.Registers := (others => 0);

      --  Set up the stack

      C.Stack := new Stack_Type (1 .. Stack_Aligned_Size);

      --  This pattern will help figuring out what is happening when debugging

      Push (16#01020304_05060708#);

      --  We want the switch subprogram to return to the top wrapper in order
      --  to properly start execution.

      C.Registers (RBP) := To_Integer (C.Stack (Cursor)'Address);
      C.Registers (RSP) := C.Registers (RBP);
      Push (To_Integer (Top_Wrapper'Address));

      --  Set up Top_Wrapper's argument

      C.Registers (RDI) := To_Integer (C'Address);

      --  The coroutine is now ready to switch to. Let the user do that
      --  whenever he wants.

      C.Registers (RAX) := 16#ABABABAB_ABABABAB#;
   end Spawn;

   ------------
   -- Switch --
   ------------

   procedure Switch (C : in out Coroutine) is
   begin
      if C = Current_Coroutine.all then
         raise Program_Error with "Trying to switch in the same coroutine";
      elsif not C.Alive then
         raise Program_Error with "Trying to switch to a dead coroutine";
      end if;

      Switch_Helper (C'Address);

      if Previous_Coroutine_Ptr.To_Clean then
         Free (Previous_Coroutine_Ptr.Stack);
         if not Is_Null_Occurrence (Previous_Coroutine_Ptr.Exc) then
            pragma Assert (Is_Null_Occurrence (C.Exc));
            Reraise_And_Clean (Previous_Coroutine_Ptr.Exc);
         end if;
      end if;

      if not Is_Null_Occurrence (Current_Coroutine.Exc) then
         Reraise_And_Clean (Current_Coroutine.Exc);
      end if;
   end Switch;

   -----------
   -- Alive --
   -----------

   function Alive (C : in out Coroutine) return Boolean is
   begin
      return C = Main_Coroutine.all or else C.Stack /= null;
   end Alive;

   ----------
   -- Kill --
   ----------

   procedure Kill (C : in out Coroutine) is
   begin
      if C = Main_Coroutine.all then
         raise Program_Error with "Cannot kill the main coroutine";

      elsif not C.Alive then
         raise Program_Error with "Coroutine already killed";
      end if;

      begin
         raise Abort_Coroutine;
      exception
         when Exc : Abort_Coroutine =>
            Save_Occurrence (C.Exc, Exc);
      end;
      C.Switch;

      Free (C.Stack);
   end Kill;

   -----------------------
   -- Current_Coroutine --
   -----------------------

   function Current_Coroutine return Coroutine_Access is
   begin
      return Current_Coroutine_Ptr;
   end Current_Coroutine;

   --------------------
   -- Main_Coroutine --
   --------------------

   function Main_Coroutine return Coroutine_Access is
   begin
      return Main_Coroutine_Obj'Access;
   end Main_Coroutine;

   -----------------
   -- Top_Wrapper --
   -----------------

   procedure Top_Wrapper (C : in out Coroutine'Class) is
   begin
      Coroutine_Wrapper (C);
   end Top_Wrapper;

   -----------------------
   -- Coroutine_Wrapper --
   -----------------------

   procedure Coroutine_Wrapper (C : in out Coroutine'Class) is
   begin
      --  When leaving Callee, the coroutine is about to abort, so the
      --  coroutine we will be switching to must clean this coroutine.

      begin
         C.Run;
      exception
         when Abort_Coroutine =>
            C.To_Clean := True;
            Previous_Coroutine_Ptr.Switch;

         when Exc : others =>
            Save_Occurrence (C.Exc, Exc);
            C.To_Clean := True;
            Previous_Coroutine_Ptr.Switch;
      end;

      C.To_Clean := True;
      Main_Coroutine.Switch;
   end Coroutine_Wrapper;

   -----------------------
   -- Reraise_And_Clean --
   -----------------------

   procedure Reraise_And_Clean (Exc : in out Exception_Occurrence) is
      Saved_Exc : Exception_Occurrence;
   begin
      Save_Occurrence (Saved_Exc, Exc);
      Save_Occurrence (Exc, Null_Occurrence);
      Reraise_Occurrence (Saved_Exc);
   end Reraise_And_Clean;

end Coroutines;
