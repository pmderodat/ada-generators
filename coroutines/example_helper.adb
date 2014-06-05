with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;

package body Example_Helper is

   type My_Coroutine is new Coroutines.Coroutine with null record;

   procedure Run (C : in out My_Coroutine) is
   begin
      Put_Line ("Hello 1/2 from coroutine");
      Main_Coroutine.Switch;
      Put_Line ("Hello 2/2 from coroutine");
      Put_Line ("Bye!");
      raise Program_Error;
   end Run;

   procedure Main is
      C1 : My_Coroutine;
   begin
      C1.Spawn;
      Put_Line ("Hello 1/3 from main");
      C1.Switch;
      Put_Line ("Hello 2/3 from main");
      begin
         C1.Switch;
      exception
         when Program_Error =>
            Put_Line ("Received error from coroutine!");
      end;
      --  C1.Kill;
      Put_Line ("Hello 3/3 from main");
   end Main;

end Example_Helper;
