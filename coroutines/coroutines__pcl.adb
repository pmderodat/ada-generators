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

   package Conversions is new  System.Address_To_Access_Conversions
     (Coroutine'Class);

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
      Data       => Convert (PCL.Current),
      Is_Main    => True,
      Is_Started => True,
      To_Clean   => False,
      Exc        => <>);
   Previous_Coroutine_Ptr : Coroutine_Access := Main_Coroutine_Obj'Access;

   Abort_Coroutine : exception;
   --  Users should not be able to stop coroutine abortion. Use
   --  Standard'Abort_Signal instead???

   function "=" (Left, Right : Coroutine'Class) return Boolean is
     (Left'Address = Right'Address);

   procedure Coroutine_Wrapper (Data : System.Address)
     with Convention => C;
   --  Wrapper for coroutines execution: landing pad, catch exceptions, manage
   --  state finalization.

   procedure Reraise_And_Clean (Exc : in out Exception_Occurrence);

   procedure Reset (C : in out Coroutine);

   -----------
   -- Reset --
   -----------

   procedure Reset (C : in out Coroutine) is
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

   procedure Initialize (C : in out Coroutine) is
   begin
      C.Data := Null_Address;
      C.Is_Main := False;
      C.To_Clean := False;
      C.Is_Started := False;
      Save_Occurrence (C.Exc, Null_Occurrence);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (C : in out Coroutine) is
   begin
      if C /= Main_Coroutine.all and then C.Data /= Null_Address then
         C.Kill;
      end if;
   end Finalize;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (C          : in out Coroutine;
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
         raise Program_Error with "Coroutine already running";
      end if;

      Coro := Create
        (Func  => Coroutine_Wrapper'Access,
         Data  => C'Address,
         Stack => Null_Address,
         Size  => Integer (Stack_Size));
      if Coro = Null_Coroutine then
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

   procedure Switch (C : in out Coroutine) is
   begin
      if C.Data = Current_Coroutine.Data then
         raise Program_Error with "Trying to switch in the same coroutine";
      elsif not C.Alive then
         raise Program_Error with "Trying to switch to a dead coroutine";
      end if;

      --  From the next coroutine to run's point of view, the current coroutine
      --  is what Previous_Coroutine_Ptr shall be.

      Previous_Coroutine_Ptr := Current_Coroutine;
      Call (Convert (C.Data));

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
      return C.Data /= Null_Address;
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

   function Current_Coroutine return Coroutine_Access is
      Current_Coroutine : constant PCL.Coroutine := Current;
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Coroutine_Access);
   begin
      if Current_Coroutine = Convert (Main_Coroutine_Obj.Data) then
         return Main_Coroutine_Obj'Access;
      else
         return Convert (Get_Data (Current_Coroutine));
      end if;
   end Current_Coroutine;

   --------------------
   -- Main_Coroutine --
   --------------------

   function Main_Coroutine return Coroutine_Access is
   begin
      return Main_Coroutine_Obj'Access;
   end Main_Coroutine;

   -----------------------
   -- Coroutine_Wrapper --
   -----------------------

   procedure Coroutine_Wrapper (Data : System.Address) is
      package Conversions is new  System.Address_To_Access_Conversions
        (Coroutine'Class);
      C : access Coroutine'Class := Conversions.To_Pointer (Data);
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
