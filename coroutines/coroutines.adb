with Ada.Exceptions; use Ada.Exceptions;
with Ada.Exceptions.Is_Null_Occurrence;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with System; use System;
with System.Address_To_Access_Conversions;

with PCL; use PCL;

package body Coroutines is

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, PCL.Coroutine);

   function Convert is new Ada.Unchecked_Conversion
     (PCL.Coroutine, System.Address);

   package Conversions is new System.Address_To_Access_Conversions
     (Coroutine_Internal);

   function Get_Coroutine (C : Coroutine_Internal_Access) return Coroutine;

   --------------------
   -- Main coroutine --
   --------------------

   Main_Coroutine_Internal : aliased Coroutine_Internal :=
     (Ada.Finalization.Limited_Controlled with
      D          => null,
      Ref_Count  => 1,
      Parent     => (Ada.Finalization.Controlled with Coroutine => null),
      Data       => Convert (PCL.Current),
      Is_Main    => True,
      Is_Started => True,
      To_Clean   => False,
      Exc        => <>);
   Previous_Coroutine : Coroutine_Internal_Access :=
     Main_Coroutine_Internal'Access;

   Abort_Coroutine : exception;
   --  Users should not be able to stop coroutine abortion. Use
   --  Standard'Abort_Signal instead???

   procedure Coroutine_Wrapper (Data : System.Address)
     with Convention => C;
   --  Wrapper for coroutines execution: landing pad, catch exceptions, manage
   --  state finalization.

   procedure Reraise_And_Clean (Exc : in out Exception_Occurrence);
   --  Re-raise Exc while leaving Exc as a null occurrence

   procedure Reset (C : in out Coroutine_Internal);
   --  Assuming that C is not running anymore, free associated resources and
   --  reset flags.

   function Current_Coroutine_Internal return Coroutine_Internal_Access;

   -------------------
   -- Get_Coroutine --
   -------------------

   function Get_Coroutine (C : Coroutine_Internal_Access) return Coroutine is
   begin
      C.Ref_Count := C.Ref_Count + 1;
      return (Ada.Finalization.Controlled with
                Coroutine => C);
   end Get_Coroutine;

   --------------------------------
   -- Current_Coroutine_Internal --
   --------------------------------

   function Current_Coroutine_Internal return Coroutine_Internal_Access is
      Current_Coroutine : constant PCL.Coroutine := Current;
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Coroutine_Internal_Access);
   begin
      if Current_Coroutine = Convert (Main_Coroutine_Internal.Data) then
         return Main_Coroutine_Internal'Access;
      else
         return Convert (Get_Data (Current_Coroutine));
      end if;
   end Current_Coroutine_Internal;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Coroutine) return Boolean is
   begin
      return Left.Coroutine = Right.Coroutine;
   end "=";

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (C : in out Coroutine) is
   begin
      C.Coroutine := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (C : in out Coroutine) is
   begin
      if C.Coroutine /= null then
         C.Coroutine.Ref_Count := C.Coroutine.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (C : in out Coroutine) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Coroutine_Internal, Coroutine_Internal_Access);
   begin
      if C.Coroutine = null then
         return;
      end if;

      C.Coroutine.Ref_Count := C.Coroutine.Ref_Count - 1;
      if C.Coroutine /= Main_Coroutine_Internal'Access
         and then C.Coroutine.Ref_Count = 0
      then
         Free (C.Coroutine);
      end if;
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create (D : Delegate_Access) return Coroutine is
      pragma Assert (D /= null);

      C_Int : Coroutine_Internal_Access := new Coroutine_Internal (D);
   begin
      C_Int.Parent := Current_Coroutine;
      return Get_Coroutine (C_Int);
   end Create;

   -----------
   -- Alive --
   -----------

   function Alive (C : Coroutine) return Boolean is
   begin
      return C.Coroutine.Alive;
   end Alive;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (C          : Coroutine;
      Stack_Size : System.Storage_Elements.Storage_Offset := 2**16) is
   begin
      C.Coroutine.Spawn;
   end Spawn;

   ------------
   -- Switch --
   ------------

   procedure Switch (C : Coroutine) is
   begin
      C.Coroutine.Switch;
   end Switch;

   ----------
   -- Kill --
   ----------

   procedure Kill (C : Coroutine) is
   begin
      C.Coroutine.Kill;
   end Kill;

   -----------
   -- Reset --
   -----------

   procedure Reset (C : in out Coroutine_Internal) is
   begin
      if C.Data /= Null_Address then
         Delete (Convert (C.Data));
         C.Data := Null_Address;
      end if;
      C.Is_Started := False;
      C.To_Clean := False;
   end Reset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : in out Coroutine_Internal) is
   begin
      C.Ref_Count := 0;
      C.Parent := (Ada.Finalization.Controlled with Coroutine => null);
      C.Data := Null_Address;
      C.Is_Main := False;
      C.To_Clean := False;
      C.Is_Started := False;
      Save_Occurrence (C.Exc, Null_Occurrence);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (C : in out Coroutine_Internal) is
      subtype Delegate_Class_Wide is Delegate'Class;
      procedure Free is new Ada.Unchecked_Deallocation
        (Delegate_Class_Wide, Delegate_Access);
      D : Delegate_Access := C.D;
   begin
      if not C.Is_Main then
         if C.Alive then
            C.Kill;
         end if;
         Free (D);
      end if;
   end Finalize;

   -----------
   -- Alive --
   -----------

   function Alive (C : in out Coroutine_Internal) return Boolean is
   begin
      return C.Data /= Null_Address;
   end Alive;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (C          : in out Coroutine_Internal;
      Stack_Size : System.Storage_Elements.Storage_Offset := 2**16)
   is
      procedure Discard (A : System.Address);

      -------------
      -- Discard --
      -------------

      procedure Discard (A : System.Address) is
      begin
         null;
      end Discard;

      Coro : PCL.Coroutine;
   begin
      if C.Alive then
         raise Coroutine_Error with "Coroutine already spawed";
      end if;

      Coro := Create
        (Func  => Coroutine_Wrapper'Access,
         Data  => C'Address,
         Stack => Null_Address,
         Size  => Integer (Stack_Size));
      if Coro = PCL.Null_Coroutine then
         --  TODO: check errno, etc.
         raise Program_Error with "PCL.Create failed";
      end if;
      C.Data := Convert (Coro);
      C.Is_Main := False;
      C.To_Clean := False;
      Save_Occurrence (C.Exc, Null_Occurrence);
   end Spawn;

   ------------
   -- Switch --
   ------------

   procedure Switch (C : in out Coroutine_Internal) is
   begin
      if C.Data = Convert (Current) then
         raise Coroutine_Error with "Trying to switch to the same coroutine";
      elsif not C.Alive then
         raise Coroutine_Error with "Trying to switch to a dead coroutine";
      end if;

      --  From the next coroutine to run's point of view, the current coroutine
      --  is what Previous_Coroutine_Ptr shall be.

      Previous_Coroutine := Current_Coroutine_Internal;
      Call (Convert (C.Data));

      if Previous_Coroutine.To_Clean then
         Reset (Previous_Coroutine.all);
         if not Is_Null_Occurrence (Previous_Coroutine.Exc) then
            pragma Assert (Is_Null_Occurrence (C.Exc));
            Reraise_And_Clean (Previous_Coroutine.Exc);
         end if;
      end if;

      if not Is_Null_Occurrence (Current_Coroutine_Internal.Exc) then
         Reraise_And_Clean (Current_Coroutine_Internal.Exc);
      end if;
   end Switch;

   ----------
   -- Kill --
   ----------

   procedure Kill (C : in out Coroutine_Internal) is
   begin
      if C'Unrestricted_Access = Main_Coroutine.Coroutine then
         raise Coroutine_Error with "Cannot kill the main coroutine";

      elsif not C.Alive then
         raise Coroutine_Error with "Coroutine already killed";
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

      --  The following will switch to C, raise an exception that will unwind
      --  its stack. Then, C's Coroutine_Wrapper instance will switch back to
      --  the current coroutine.

      C.Switch;

      --  When coming back from C, the Switch routine is supposed to clean
      --  *and* delete C's PCL coroutine, so we are done.
   end Kill;

   -----------------------
   -- Current_Coroutine --
   -----------------------

   function Current_Coroutine return Coroutine is
   begin
      return Get_Coroutine (Current_Coroutine_Internal);
   end Current_Coroutine;

   --------------------
   -- Main_Coroutine --
   --------------------

   function Main_Coroutine return Coroutine is
   begin
      return Get_Coroutine (Main_Coroutine_Internal'Access);
   end Main_Coroutine;

   -----------------------
   -- Coroutine_Wrapper --
   -----------------------

   procedure Coroutine_Wrapper (Data : System.Address) is
      package Conversions is new System.Address_To_Access_Conversions
        (Coroutine_Internal);
      C : access Coroutine_Internal := Conversions.To_Pointer (Data);

   begin
      C.Is_Started := True;

      --  When leaving Callee, the coroutine is about to abort, so the
      --  coroutine we will be switching to must clean this coroutine.

      begin
         C.D.Run;
      exception
         when Abort_Coroutine =>
            null;

         when Exc : others =>
            Save_Occurrence (C.Exc, Exc);
      end;

      C.To_Clean := True;

      --  Get the nearest parent coroutine still alive and resume execution in
      --  it.

      declare
         Alive_Parent : Coroutine_Internal_Access := C.Parent.Coroutine;
      begin
         while not Alive_Parent.Alive loop
            Alive_Parent := Alive_Parent.Parent.Coroutine;
         end loop;
         Alive_Parent.Switch;
      end;
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
