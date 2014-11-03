with Ada.Unchecked_Deallocation;

package body Generators is

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (G : in out Generator) is
   begin
      G.Generator := null;
      G.Weak := False;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (G : in out Generator) is
   begin
      if G.Generator /= null then
         G.Generator.Ref_Count := G.Generator.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (G : in out Generator) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Generator_Internal, Generator_Internal_Access);
   begin
      if G.Generator = null or else G.Weak then
         return;
      end if;

      G.Generator.Ref_Count := G.Generator.Ref_Count - 1;
      if G.Generator.Ref_Count = 0 then
         Free (G.Generator);
      end if;
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create (D : Delegate_Access) return Generator is
      use type Coroutines.Delegate_Access;
      pragma Assert (D /= null);

      G_Int : Generator_Internal_Access := new Generator_Internal;
   begin
      G_Int.Ref_Count := 1;
      G_Int.Delegate := D;
      G_Int.Coroutine :=
        Coroutines.Create (new Generator_Delegate'(Generator => G_Int));
      G_Int.Coroutine.Spawn;
      return (Ada.Finalization.Controlled with
              Generator => G_Int,
              Weak      => False);
   end Create;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (G : in out Generator_Internal) is
   begin
      G.Ref_Count := 0;
      G.Caller := Coroutines.Current_Coroutine;
      G.State := Waiting;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (G : in out Generator_Internal) is
      subtype Delegate_Class_Wide is Delegate'Class;
      procedure Free is new Ada.Unchecked_Deallocation
        (Delegate_Class_Wide, Delegate_Access);
   begin
      G.Coroutine := Coroutines.Null_Coroutine;
      G.Caller := Coroutines.Null_Coroutine;
      G.State := Returning;
      Free (G.Delegate);
   end Finalize;

   ---------
   -- Run --
   ---------

   overriding
   procedure Run (D : in out Generator_Delegate) is
      G_Int : constant Generator_Internal_Access := D.Generator;
      G     : Generator :=
        (Ada.Finalization.Controlled with
         Generator => G_Int,
         Weak      => True);
      --  If a generator holds a reference to itself, it will not be garbage
      --  collected until its execution completes.
   begin
      begin
         G_Int.Delegate.Generate (Generator'Class (G));
      exception
         when others =>
            G_Int.State := Returning;
            raise;
      end;
      G_Int.State := Returning;
   end Run;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (G : Generator) return Boolean is
      G_Int : constant Generator_Internal_Access := G.Generator;
   begin
      case G_Int.State is
         when Waiting =>
            null;
         when Yielding =>
            return True;
         when Returning =>
            return False;
      end case;

      G_Int.Coroutine.Switch;

      case G_Int.State is
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

   function Next (G : Generator) return T is
      G_Int : constant Generator_Internal_Access := G.Generator;
   begin
      case G_Int.State is
         when Waiting | Returning =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            G_Int.State := Waiting;
            return G_Int.Yield_Value;
      end case;
   end Next;

   -----------
   -- Yield --
   -----------

   procedure Yield (G : Generator; Value : T) is
      G_Int : constant Generator_Internal_Access := G.Generator;
   begin
      G_Int.State := Yielding;
      G_Int.Yield_Value := Value;
      G_Int.Caller.Switch;
   end Yield;

   -----------
   -- First --
   -----------

   function First (G : Generator) return Cursor_Type is
   begin
      return (others => <>);
   end First;

   ----------
   -- Next --
   ----------

   function Next (G : Generator; C : Cursor_Type) return Cursor_Type is
      G_Int : constant Generator_Internal_Access := G.Generator;
   begin
      case G_Int.State is
         when Waiting =>
            null;
         when Yielding =>
            G_Int.State := Waiting;
         when Returning =>
            raise Program_Error with "Unreachable state";
      end case;
      return G.First;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (G : Generator; C : Cursor_Type) return Boolean
   is
   begin
      return G.Has_Next;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (G : Generator; C : Cursor_Type) return T is
      G_Int : constant Generator_Internal_Access := G.Generator;
   begin
      case G_Int.State is
         when Waiting | Returning =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            return G_Int.Yield_Value;
      end case;
   end Element;

end Generators;
