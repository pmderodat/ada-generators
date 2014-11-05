with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;

--  Test that the simultaneous usage of coroutines and secondary stack does not
--  cause trouble.

procedure Test_Secondary_Stack is

   type Delegate is new Coroutines.Delegate with record
      Name : Character;
      Next : Coroutine;
   end record;
   overriding procedure Run (D : in out Delegate);

   function Letters (N : Natural; C : Character) return String;
   procedure Log (D : in out Delegate'Class; Message : String);
   function Switch_Identity (D : in out Delegate'Class; S : String)
                             return String;

   -------------
   -- Letters --
   -------------

   function Letters (N : Natural; C : Character) return String is
   begin
      return (1 .. N => C);
   end Letters;

   ---------
   -- Log --
   ---------

   procedure Log (D : in out Delegate'Class; Message : String) is
   begin
      Put_Line (D.Name & ": " & Message);
   end Log;

   ---------------------
   -- Switch_Identity --
   ---------------------

   function Switch_Identity (D : in out Delegate'Class; S : String)
                             return String
   is
   begin
      Log (D, "Switch_Identity called: " & S);
      D.Next.Switch;
      Log (D, "Switch_Identity resumed: " & S);
      return S;
   end Switch_Identity;

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Delegate) is

      --  Two coroutines execute this routine and the execution will go back
      --  and forth between them. The aim is to mix secondary stack usage in
      --  an attempt to raise a possible issue in secondary stack handling for
      --  coroutines.

      procedure Helper;

      ------------
      -- Helper --
      ------------

      procedure Helper is
      begin
         Log (D, Switch_Identity (D, Letters (10, D.Name)));
      end Helper;

   begin
      Log (D, "Hello !");

      if D.Name = 'A' then

         --  If the coroutines implementation is incorrect (i.e. if it make
         --  coroutines share the same secondary stack), the following is
         --  going to happen.

         --  The following call will switch to B. B will start to allocate
         --  things on the secondary stack and then will switch to A (i.e.
         --  execution will resume here).

         Helper;

         --  Then, the following calls will corrupt the secondary stack shared
         --  with B and then resume to B. B will use corrupted bytes and will
         --  print them.

         Log (D, Letters (100, 'A') (1 .. 10));
         D.Next.Switch;

      else
         Helper;
      end if;

      Log (D, "about to terminate");
   end Run;

   D_A : constant access Delegate := new Delegate'
     (Name => 'A', Next => Null_Coroutine);
   D_B : constant access Delegate := new Delegate'
     (Name => 'B', Next => Null_Coroutine);
   A, B : Coroutine;

begin
   A := Create (Delegate_Access (D_A));
   B := Create (Delegate_Access (D_B));
   D_A.Next := B;
   D_B.Next := A;

   A.Spawn;
   B.Spawn;
   Put_Line ("Main: about to switch to A");
   A.Switch;
   Put_Line ("Main: about to terminate");
end Test_Secondary_Stack;
