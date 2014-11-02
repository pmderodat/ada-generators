with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Null_Delegate) is
   begin
      null;
   end Run;

   ---------
   -- Run --
   ---------

   overriding procedure Run (D : in out Hello_World_Delegate) is
   begin
      for I in 1 .. D.Iterations loop
         Put_Line ("Hello, world!");
         D.Caller.Switch;
      end loop;
      Put_Line ("Last Hello, world!");
   end Run;

end Support;
