package body Support is

   --------------
   -- Generate --
   --------------

   overriding procedure Generate (C : in out Counter_Infinity) is
      I : Integer := C.First;
   begin
      loop
         C.Yield (I);
         I := I + 1;
      end loop;
   end Generate;


   --------------
   -- Generate --
   --------------

   overriding procedure Generate (C : in out Counter_Finite) is
      I : Integer := 1;
   begin
      while I <= C.Last loop
         C.Yield (I);
         I := I + 1;
      end loop;
   end Generate;

end Support;
