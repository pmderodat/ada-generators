package body Generators is

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (G : in out Generator) is
      use System.Storage_Elements;
   begin
      Coroutines.Initialize (Coroutines.Coroutine (G));
      G.Caller := Coroutines.Current_Coroutine;
      G.State := Waiting;
      G.Spawn;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (G : in out Generator) is
   begin
      Coroutines.Finalize (Coroutines.Coroutine (G));
      G.State := Returning;
   end Finalize;

   ---------
   -- Run --
   ---------

   overriding
   procedure Run (G : in out Generator) is
      G_Class : Generator'Class renames Generator'Class (G);
   begin
      begin
         G_Class.Generate;
      exception
         when others =>
            G.State := Returning;
            raise;
      end;
      G.State := Returning;
   end Run;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (G : in out Generator) return Boolean is
   begin
      case G.State is
         when Waiting =>
            null;
         when Yielding =>
            return True;
         when Returning =>
            return False;
      end case;

      G.Switch;

      case G.State is
         when Waiting =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            return True;
         when Returning =>
            return False;
      end case;
   end Has_Next;

   ----------
   -- Next --
   ----------

   function Next (G : in out Generator) return T is
   begin
      case G.State is
         when Waiting | Returning =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            G.State := Waiting;
            return G.Yield_Value;
      end case;
   end Next;

   -----------
   -- Yield --
   -----------

   procedure Yield (G : in out Generator; Value : T) is
   begin
      G.State := Yielding;
      G.Yield_Value := Value;
      G.Caller.Switch;
   end Yield;

   -----------
   -- First --
   -----------

   function First (G : in out Generator) return Cursor_Type is
   begin
      return (others => <>);
   end First;

   ----------
   -- Next --
   ----------

   function Next (G : in out Generator; C : Cursor_Type) return Cursor_Type is
   begin
      case G.State is
         when Waiting =>
            null;
         when Yielding =>
            G.State := Waiting;
         when Returning =>
            raise Program_Error with "Unreachable state";
      end case;
      return G.First;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (G : in out Generator; C : Cursor_Type) return Boolean
   is
   begin
      return G.Has_Next;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (G : in out Generator; C : Cursor_Type) return T is
   begin
      case G.State is
         when Waiting | Returning =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            return G.Yield_Value;
      end case;
   end Element;

end Generators;
