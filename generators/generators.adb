with Ada.Unchecked_Deallocation;

package body Generators is

   -------------
   -- Is_Null --
   -------------

   function Is_Null (G : Generator) return Boolean is
   begin
      return G.Generator = null;
   end Is_Null;

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

   function Create (D                  : Delegate_Access;
                    Transfer_Ownership : Boolean := True) return Generator is
      pragma Assert (D /= null);

      G_Int      : constant Generator_Internal_Access :=
        new Generator_Internal;
      G_Delegate : constant Generator_Delegate_Access :=
        new Generator_Delegate'(Generator => G_Int);
   begin
      G_Int.Ref_Count := 1;
      G_Int.Delegate := D;
      G_Int.Owns_Delegate := Transfer_Ownership;
      G_Int.Coroutine :=
        Coroutines.Create (Coroutines.Delegate_Access (G_Delegate));
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
      if G.Owns_Delegate then
         Free (G.Delegate);
      end if;
   end Finalize;

   ---------
   -- Run --
   ---------

   overriding
   procedure Run (D : in out Generator_Delegate) is
      G_Int : constant Generator_Internal_Access := D.Generator;
      G     : constant Generator :=
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

      --  We do not want to resume to the parent coroutine. We want instead to
      --  resume to the last coroutine that invoked this generator, so do not
      --  rely on usual coroutine completion mechanism.

      G_Int.State := Returning;
      G_Int.Caller.Switch;
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

      G_Int.Caller := Coroutines.Current_Coroutine;
      G_Int.Coroutine.Switch;
      G_Int.Caller := Coroutines.Null_Coroutine;

      case G_Int.State is
         when Waiting =>
            raise Program_Error with "Unreachable state";
         when Yielding =>
            return True;
         when Returning =>
            --  We do not want to rely on usual coroutine completion mechanism
            --  (see Run), so kill completed generators as soon as possible.

            if G_Int.Coroutine.Alive then
               G_Int.Coroutine.Kill;
            end if;
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
      pragma Unreferenced (G);
   begin
      return (null record);
   end First;

   ----------
   -- Next --
   ----------

   function Next (G : Generator; C : Cursor_Type) return Cursor_Type is
      pragma Unreferenced (C);
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
      pragma Unreferenced (C);
   begin
      return G.Has_Next;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (G : Generator; C : Cursor_Type) return T is
      pragma Unreferenced (C);
      G_Int : constant Generator_Internal_Access := G.Generator;
   begin
      case G_Int.State is
         when Returning =>
            raise Program_Error with "Unreachable state";

         when Waiting | Yielding =>

            --  Switch to Waiting state so that we don't require a call to Next
            --  in order to go to the next element. This is useful to resume
            --  iteration with a FOR loop.

            G_Int.State := Waiting;

            return G_Int.Yield_Value;
      end case;
   end Element;

end Generators;
