with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with System; use System;
with System.Address_To_Access_Conversions;
with Ada.Exceptions.Is_Null_Occurrence;

package body Coroutines is

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

   type Coroutine_Data is record
      Stack     : Stack_Access;
      Registers : Register_Base;
   end record;
   type Coroutine_Data_Access is access all Coroutine_Data;

   package Conversions is new System.Address_To_Access_Conversions
     (Coroutine_Data);

   function Get_Data (C : Coroutine) return Coroutine_Data_Access is
     (Coroutine_Data_Access (Conversions.To_Pointer (C.Data)));

   procedure Free is new Ada.Unchecked_Deallocation
     (Coroutine_Data, Coroutine_Data_Access);

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

   Main_Coroutine_Data : aliased Coroutine_Data :=
     (Stack     => null,
      Registers => <>);
   Main_Coroutine_Obj  : aliased Main_Coroutine_Type :=
     (Ada.Finalization.Limited_Controlled with
      Data       => Main_Coroutine_Data'Address,
      Is_Main    => True,
      Is_Started => True,
      To_Clean   => False,
      Exc        => <>);

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
   --  Re-raise Exc while leaving Exc as a null occurrence

   procedure Reset (C : in out Coroutine);
   --  Assuming that C is not running anymore, free associated resources and
   --  reset flags.

   -----------
   -- Reset --
   -----------

   procedure Reset (C : in out Coroutine) is
   begin
      if Get_Data (C) /= null then
         Free (Get_Data (C).Stack);
      end if;
      C.Is_Started := False;
      C.To_Clean := False;
   end Reset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : in out Coroutine) is
      Data : Coroutine_Data_Access :=
        new Coroutine_Data'(Stack => null, Registers => <>);
   begin
      C.Data := Conversions.To_Address (Data.all'Access);
      C.Is_Started := False;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (C : in out Coroutine) is
      Data : Coroutine_Data_Access := Get_Data (C);
   begin
      if C /= Main_Coroutine.all then
         if Data.Stack /= null then
            C.Kill;
            Free (Data.Stack);
         end if;
         Free (Data);
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

      Data : Coroutine_Data renames Get_Data (C).all;

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
         Ptr : constant Stack_Pointer := Data.Stack (Cursor)'Access;
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

      Data.Registers := (others => 0);

      --  Set up the stack

      Data.Stack := new Stack_Type (1 .. Stack_Aligned_Size);

      --  This pattern will help figuring out what is happening when debugging

      Push (16#01020304_05060708#);

      --  We want the switch subprogram to return to the top wrapper in order
      --  to properly start execution.

      Data.Registers (RBP) := To_Integer (Data.Stack (Cursor)'Address);
      Data.Registers (RSP) := Data.Registers (RBP);
      Push (To_Integer (Top_Wrapper'Address));

      --  Set up Top_Wrapper's argument

      Data.Registers (RDI) := To_Integer (C'Address);

      --  The coroutine is now ready to switch to. Let the user do that
      --  whenever he wants.

      Data.Registers (RAX) := 16#ABABABAB_ABABABAB#;
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
         Reset (C);
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
      return C = Main_Coroutine.all or else Get_Data (C).Stack /= null;
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

      if not C.Is_Started then
         Reset (C);
         return;
      end if;

      begin
         raise Abort_Coroutine;
      exception
         when Exc : Abort_Coroutine =>
            Save_Occurrence (C.Exc, Exc);
      end;
      C.Switch;

      Free (Get_Data (C).Stack);
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
      Switch_To_Previous : Boolean := False;

   begin
      C.Is_Started := True;

      --  When leaving Callee, the coroutine is about to abort, so the
      --  coroutine we will be switching to must clean this coroutine.

      begin
         C.Run;
      exception
         when Abort_Coroutine =>
            Switch_To_Previous := True;

         when Exc : others =>
            Save_Occurrence (C.Exc, Exc);
            Switch_To_Previous := True;
      end;

      C.To_Clean := True;
      if Switch_To_Previous then
         Previous_Coroutine_Ptr.Switch;
      else
         Main_Coroutine.Switch;
      end if;
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
