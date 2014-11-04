with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;

--  Test where the completion of a coroutine resumes execution to when the
--  parent coroutine is dead.

--  When a coroutine terminates while its creator coroutine is dead but its
--  "great-parent" is still aive, we expect the execution to resume to the
--  "great-parent".

procedure Test_Resume_Parent_Dead is

   type Parent_Delegate is new Delegate with null record;
   overriding procedure Run (D : in out Parent_Delegate);

   type Child_Delegate is new Delegate with null record;
   overriding procedure Run (D : in out Child_Delegate);

   Parent_Coroutine : Coroutine;
   Child_Coroutine : Coroutine;

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Parent_Delegate) is
      pragma Unreferenced (D);
      Child_D : constant access Child_Delegate := new Child_Delegate;
   begin
      Child_Coroutine := Create (Delegate_Access (Child_D));
      Child_Coroutine.Spawn;

      Put_Line ("Parent: switching to child coroutine");
      Child_Coroutine.Switch;

      Put_Line ("Parent: about to terminate");
   end Run;

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Child_Delegate) is
      pragma Unreferenced (D);
   begin
      Put_Line ("Child: switching back to parent coroutine");
      Parent_Coroutine.Switch;

      Put_Line ("Child: about to terminate");
   end Run;

   D : constant access Parent_Delegate := new Parent_Delegate;

begin
   Parent_Coroutine := Create (Delegate_Access (D));
   Parent_Coroutine.Spawn;

   Put_Line ("Main: switching to parent coroutine");
   Parent_Coroutine.Switch;

   Put_Line ("Main: switching to child coroutine");
   Child_Coroutine.Switch;

   Put_Line ("Main: about to terminate");
end Test_Resume_Parent_Dead;
