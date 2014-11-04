package body Support is

   --------------
   -- Generate --
   --------------

   overriding procedure Generate (D : in out Counter_Infinity;
                                  G : Int_Generators.Generator'Class) is
      I : Integer := D.First;
   begin
      loop
         G.Yield (I);
         I := I + 1;
      end loop;
   end Generate;

   --------------
   -- Generate --
   --------------

   overriding procedure Generate (D : in out Counter_Finite;
                                  G : Int_Generators.Generator'Class) is
      I : Integer := 1;
   begin
      while I <= D.Last loop
         G.Yield (I);
         I := I + 1;
      end loop;
   end Generate;

   --------------
   -- Generate --
   --------------

   overriding procedure Generate (D : in out Doubles;
                                  G : Int_Generators.Generator'Class) is
   begin
      for I of D.G loop
         G.Yield (2 * I);
      end loop;
   end Generate;

end Support;
