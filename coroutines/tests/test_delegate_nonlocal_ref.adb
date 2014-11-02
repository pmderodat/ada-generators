with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;

--  Test performing non-local variable references from a local delegate

procedure Test_Delegate_Nonlocal_Ref is
   type Local_Delegate is new Delegate with null record;
   overriding procedure Run (D : in out Local_Delegate);

   I : Natural := 0;

   overriding procedure Run (D : in out Local_Delegate) is
   begin
      I := 1;
   end Run;

   D : access Local_Delegate := new Local_Delegate;
   C : Coroutine := Create (Delegate_Access (D));
begin
   Put_Line ("Before: I =" & Natural'Image (I));
   C.Spawn;
   C.Switch;
   Put_Line ("After: I =" & Natural'Image (I));
end Test_Delegate_Nonlocal_Ref;
