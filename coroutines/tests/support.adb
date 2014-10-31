with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   ---------
   -- Run --
   ---------

   overriding procedure Run (C : in out Null_Coroutine) is
   begin
      null;
   end Run;

   ---------
   -- Run --
   ---------

   overriding procedure Run (C : in out Hello_World_Coroutine) is
   begin
      for I in 1 .. C.Iterations loop
         Put_Line ("Hello, world!");
         C.Caller.Switch;
      end loop;
      Put_Line ("Last Hello, world!");
   end Run;

end Support;
