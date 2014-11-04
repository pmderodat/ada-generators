with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;
with Support; use Support;

--  Test where killing an alive coroutine outside of its parent resumes
--  execution to. Killing is always expected to resume execution to the
--  coroutine that invoked the Kill primitive.

procedure Test_Foreign_Kill is

   C_Parent : Coroutine;
   C_Child  : Coroutine;

   type Parent_Delegate is new Delegate with null record;
   overriding procedure Run (D : in out Parent_Delegate);

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Parent_Delegate) is
      pragma Unreferenced (D);
   begin
      C_Child := Create (new Hello_World_Delegate'
                           (Caller     => Current_Coroutine,
                            Iterations => 3));
      C_Child.Spawn;
      Put_Line ("Parent: about to switch to Child");
      C_Child.Switch;

      Put_Line ("Parent: about to switch to Main");
      Main_Coroutine.Switch;

      Put_Line ("Parent: about to terminate");
   end Run;

   D_Parent : constant access Parent_Delegate := new Parent_Delegate;
begin
   C_Parent := Create (Delegate_Access (D_Parent));
   C_Parent.Spawn;
   Put_Line ("Main: about to spawn Parent");
   C_Parent.Switch;

   Put_Line ("Main: about to kill Child");
   C_Child.Kill;

   Put_Line ("Main: about to terminate");
end Test_Foreign_Kill;
