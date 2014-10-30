package body Generators is

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (I : in out Generator) is
      use System.Storage_Elements;
   begin
      Coroutines.Initialize (Coroutines.Coroutine (I));
      I.Caller := Coroutines.Current_Coroutine;
      I.State := Waiting;
      I.Spawn;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (I : in out Generator) is
   begin
      Coroutines.Finalize (Coroutines.Coroutine (I));
      I.State := Returning;
   end Finalize;

   ---------
   -- Run --
   ---------

   overriding
   procedure Run (I : in out Generator) is
      I_Class : Generator'Class renames Generator'Class (I);
   begin
      begin
         I_Class.Generate;
      exception
         when others =>
            I.State := Returning;
            raise;
      end;
   end Run;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (I : in out Generator) return Boolean is
   begin
      case I.State is
         when Waiting =>
            null;
         when Yielding =>
            return True;
         when Returning =>
            return False;
      end case;

      I.Switch;

      case I.State is
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

   function Next (I : in out Generator) return T is
   begin
      case I.State is
         when Waiting | Returning =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            I.State := Waiting;
            return I.Yield_Value;
      end case;
   end Next;

   -----------
   -- Yield --
   -----------

   procedure Yield (I : in out Generator; Value : T) is
   begin
      I.State := Yielding;
      I.Yield_Value := Value;
      I.Caller.Switch;
   end Yield;

   -----------
   -- First --
   -----------

   function First (I : in out Generator) return Cursor_Type is
   begin
      return (others => <>);
   end First;

   ----------
   -- Next --
   ----------

   function Next (I : in out Generator; C : Cursor_Type) return Cursor_Type is
   begin
      case I.State is
         when Waiting =>
            null;
         when Yielding =>
            I.State := Waiting;
         when Returning =>
            raise Program_Error with "Unreachable state";
      end case;
      return I.First;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (I : in out Generator; C : Cursor_Type) return Boolean
   is
   begin
      return I.Has_Next;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (I : in out Generator; C : Cursor_Type) return T is
   begin
      case I.State is
         when Waiting | Returning =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            return I.Yield_Value;
      end case;
   end Element;

end Generators;
