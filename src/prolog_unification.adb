-- ***************************************************************************
--           Prolog Unification - Unification algorithm
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Prolog_Unification is

   procedure Bind (Var : Term_Access; Value : Term_Access; Tr : in out Trail) is
   begin
      Tr.Bindings.Append ((Variable => Var, Old_Value => Var.Bound_To));
      Var.Bound_To := Value;
   end Bind;

   ----------------
   -- Occurs_In --
   ----------------

   function Occurs_In (Var : Term_Access; T : Term_Access) return Boolean is
      Deref_T : constant Term_Access := Dereference (T);
   begin
      if Deref_T = null then
         return False;
      end if;

      case Deref_T.Kind is
         when Term_Variable =>
            return Var.Var_Id = Deref_T.Var_Id;

         when Term_Compound =>
            for I in Deref_T.Arguments.First_Index .. Deref_T.Arguments.Last_Index loop
               if Occurs_In (Var, Deref_T.Arguments (I)) then
                  return True;
               end if;
            end loop;
            return False;

         when others =>
            return False;
      end case;
   end Occurs_In;

   -----------
   -- Unify --
   -----------

   function Unify (T1, T2 : Term_Access; Tr : in out Trail) return Boolean is
      D1 : constant Term_Access := Dereference (T1);
      D2 : constant Term_Access := Dereference (T2);
   begin
      if D1 = null or else D2 = null then
         return D1 = D2;
      end if;

      --  If same object, unification succeeds
      if D1 = D2 then
         return True;
      end if;

      --  Handle variables
      if D1.Kind = Term_Variable then
         if Occurs_In (D1, D2) then
            return False;  --  Occurs check failure
         end if;
         Bind (D1, D2, Tr);
         return True;
      end if;

      if D2.Kind = Term_Variable then
         if Occurs_In (D2, D1) then
            return False;  --  Occurs check failure
         end if;
         Bind (D2, D1, Tr);
         return True;
      end if;

      --  Both are non-variables
      case D1.Kind is
         when Term_Atom =>
            return D2.Kind = Term_Atom and then
                   D1.Atom_Name = D2.Atom_Name;

         when Term_Integer =>
            return D2.Kind = Term_Integer and then
                   D1.Int_Value = D2.Int_Value;

         when Term_Float =>
            return D2.Kind = Term_Float and then
                   D1.Float_Value = D2.Float_Value;

         when Term_Nil =>
            return D2.Kind = Term_Nil;

         when Term_Compound =>
            if D2.Kind /= Term_Compound then
               return False;
            end if;

            if D1.Functor /= D2.Functor then
               return False;
            end if;

            if Natural (D1.Arguments.Length) /= Natural (D2.Arguments.Length) then
               return False;
            end if;

            for I in D1.Arguments.First_Index .. D1.Arguments.Last_Index loop
               if not Unify (D1.Arguments (I), D2.Arguments (I), Tr) then
                  return False;
               end if;
            end loop;

            return True;

         when Term_Variable =>
            --  Already handled above
            return False;
      end case;
   end Unify;

   --------------------
   -- Trail_Position --
   --------------------

   function Trail_Position (Tr : Trail) return Natural is
   begin
      return Natural (Tr.Bindings.Length);
   end Trail_Position;

   -------------------
   -- Undo_Bindings --
   -------------------

   procedure Undo_Bindings (Tr : in out Trail; Position : Natural) is
   begin
      while Natural (Tr.Bindings.Length) > Position loop
         declare
            B : constant Binding := Tr.Bindings.Last_Element;
         begin
            B.Variable.Bound_To := B.Old_Value;
            Tr.Bindings.Delete_Last;
         end;
      end loop;
   end Undo_Bindings;

   -----------------
   -- Clear_Trail --
   -----------------

   procedure Clear_Trail (Tr : in out Trail) is
   begin
      Undo_Bindings (Tr, 0);
   end Clear_Trail;

end Prolog_Unification;
