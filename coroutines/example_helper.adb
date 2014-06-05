with Ada.Text_IO; use Ada.Text_IO;

with Coroutines; use Coroutines;

package body Example_Helper is

   procedure C1_Callee (C : in out Coroutines.Coroutine) is
   begin
      Put_Line ("Hello 1/2 from coroutine");
      Main_Coroutine.Switch;
      Put_Line ("Hello 2/2 from coroutine");
      Put_Line ("Bye!");
      raise Program_Error;
   end C1_Callee;

   procedure Main is
      C1 : Coroutines.Coroutine;
   begin
      C1.Spawn (C1_Callee'Access);
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
