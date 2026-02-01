-- ***************************************************************************
--            Prolog Builtins - Built-in predicates
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

with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Long_Elementary_Functions;

package body Prolog_Builtins is

   Evaluation_Error : exception;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expr : Term_Access) return Long_Float is
      E : constant Term_Access := Dereference (Expr);
   begin
      if E = null then
         raise Evaluation_Error with "Cannot evaluate null term";
      end if;

      case E.Kind is
         when Term_Integer =>
            return Long_Float (E.Int_Value);

         when Term_Float =>
            return E.Float_Value;

         when Term_Variable =>
            raise Evaluation_Error with "Unbound variable in arithmetic expression";

         when Term_Compound =>
            declare
               Op : constant String := To_String (E.Functor);
            begin
               if Natural (E.Arguments.Length) = 2 then
                  declare
                     Left  : constant Long_Float := Evaluate (E.Arguments (0));
                     Right : constant Long_Float := Evaluate (E.Arguments (1));
                  begin
                     if Op = "+" then
                        return Left + Right;
                     elsif Op = "-" then
                        return Left - Right;
                     elsif Op = "*" then
                        return Left * Right;
                     elsif Op = "/" then
                        if Right = 0.0 then
                           raise Evaluation_Error with "Division by zero";
                        end if;
                        return Left / Right;
                     elsif Op = "mod" then
                        return Long_Float (Integer (Left) mod Integer (Right));
                     elsif Op = "**" then
                        if Right = Long_Float'Floor (Right) and then
                           Right >= 0.0 and then Right <= Long_Float (Natural'Last) then
                           return Left ** Natural (Right);
                        else
                           --  Use exp(right * ln(left)) for non-integer exponents
                           return Ada.Numerics.Long_Elementary_Functions.Exp
                                    (Right * Ada.Numerics.Long_Elementary_Functions.Log (Left));
                        end if;
                     else
                        raise Evaluation_Error with "Unknown operator: " & Op;
                     end if;
                  end;
               elsif Natural (E.Arguments.Length) = 1 then
                  declare
                     Arg : constant Long_Float := Evaluate (E.Arguments (0));
                  begin
                     if Op = "-" then
                        return -Arg;
                     elsif Op = "abs" then
                        return abs Arg;
                     elsif Op = "sqrt" then
                        return Ada.Numerics.Long_Elementary_Functions.Sqrt (Arg);
                     elsif Op = "floor" then
                        return Long_Float'Floor (Arg);
                     elsif Op = "ceiling" then
                        return Long_Float'Ceiling (Arg);
                     elsif Op = "round" then
                        return Long_Float'Rounding (Arg);
                     elsif Op = "truncate" then
                        return Long_Float'Truncation (Arg);
                     elsif Op = "float" then
                        return Arg;
                     else
                        raise Evaluation_Error with "Unknown operator: " & Op;
                     end if;
                  end;
               else
                  raise Evaluation_Error with "Invalid arity for operator: " & Op;
               end if;
            end;

         when others =>
            raise Evaluation_Error with "Cannot evaluate term: " & Term_To_String (E);
      end case;
   end Evaluate;

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Goal : Term_Access) return Boolean is
      G : constant Term_Access := Dereference (Goal);
      Functor : Unbounded_String;
      Arity   : Natural;
   begin
      if G = null then
         return False;
      end if;

      case G.Kind is
         when Term_Atom =>
            Functor := G.Atom_Name;
            Arity := 0;
         when Term_Compound =>
            Functor := G.Functor;
            Arity := Natural (G.Arguments.Length);
         when others =>
            return False;
      end case;

      declare
         F : constant String := To_String (Functor);
      begin
         --  Control
         if F = "true" and then Arity = 0 then return True; end if;
         if F = "fail" and then Arity = 0 then return True; end if;
         if F = "!" and then Arity = 0 then return True; end if;  -- Cut

         --  Comparison
         if F = "=" and then Arity = 2 then return True; end if;
         if F = "\=" and then Arity = 2 then return True; end if;
         if F = "==" and then Arity = 2 then return True; end if;
         if F = "\==" and then Arity = 2 then return True; end if;

         --  Arithmetic comparison
         if F = "is" and then Arity = 2 then return True; end if;
         if F = "=:=" and then Arity = 2 then return True; end if;
         if F = "=\=" and then Arity = 2 then return True; end if;
         if F = "<" and then Arity = 2 then return True; end if;
         if F = ">" and then Arity = 2 then return True; end if;
         if F = "=<" and then Arity = 2 then return True; end if;
         if F = ">=" and then Arity = 2 then return True; end if;

         --  Type checking
         if F = "var" and then Arity = 1 then return True; end if;
         if F = "nonvar" and then Arity = 1 then return True; end if;
         if F = "atom" and then Arity = 1 then return True; end if;
         if F = "number" and then Arity = 1 then return True; end if;
         if F = "integer" and then Arity = 1 then return True; end if;
         if F = "float" and then Arity = 1 then return True; end if;
         if F = "compound" and then Arity = 1 then return True; end if;
         if F = "is_list" and then Arity = 1 then return True; end if;

         --  I/O
         if F = "write" and then Arity = 1 then return True; end if;
         if F = "writeln" and then Arity = 1 then return True; end if;
         if F = "nl" and then Arity = 0 then return True; end if;

         --  Database manipulation
         if F = "asserta" and then Arity = 1 then return True; end if;
         if F = "assertz" and then Arity = 1 then return True; end if;
         if F = "assert" and then Arity = 1 then return True; end if;

         --  Term manipulation
         if F = "functor" and then Arity = 3 then return True; end if;
         if F = "arg" and then Arity = 3 then return True; end if;
         if F = "=.." and then Arity = 2 then return True; end if;  -- univ
         if F = "copy_term" and then Arity = 2 then return True; end if;

         return False;
      end;
   end Is_Builtin;

   ---------------------
   -- Execute_Builtin --
   ---------------------

   function Execute_Builtin
     (Goal : Term_Access;
      Tr   : in Out Trail;
      DB   : in Out Database) return Builtin_Result
   is
      G : constant Term_Access := Dereference (Goal);
      Functor : Unbounded_String;
      Arity   : Natural;
   begin
      if G = null then
         return Builtin_Failure;
      end if;

      case G.Kind is
         when Term_Atom =>
            Functor := G.Atom_Name;
            Arity := 0;
         when Term_Compound =>
            Functor := G.Functor;
            Arity := Natural (G.Arguments.Length);
         when others =>
            return Builtin_Not_Builtin;
      end case;

      declare
         F : constant String := To_String (Functor);
      begin
         --  Control predicates
         if F = "true" and then Arity = 0 then
            return Builtin_Success;
         end if;

         if F = "fail" and then Arity = 0 then
            return Builtin_Failure;
         end if;

         --  Unification
         if F = "=" and then Arity = 2 then
            if Unify (G.Arguments (0), G.Arguments (1), Tr) then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         if F = "\=" and then Arity = 2 then
            declare
               Pos : constant Natural := Trail_Position (Tr);
            begin
               if Unify (G.Arguments (0), G.Arguments (1), Tr) then
                  Undo_Bindings (Tr, Pos);
                  return Builtin_Failure;
               else
                  return Builtin_Success;
               end if;
            end;
         end if;

         --  Arithmetic
         if F = "is" and then Arity = 2 then
            declare
               Result : constant Long_Float := Evaluate (G.Arguments (1));
               Result_Term : Term_Access;
            begin
               --  Return integer if result is a whole number, otherwise float
               if Result = Long_Float'Floor (Result) and then
                  Result >= Long_Float (Integer'First) and then
                  Result <= Long_Float (Integer'Last) then
                  Result_Term := Make_Integer (Integer (Result));
               else
                  Result_Term := Make_Float (Result);
               end if;
               if Unify (G.Arguments (0), Result_Term, Tr) then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = "=:=" and then Arity = 2 then
            declare
               Left  : constant Long_Float := Evaluate (G.Arguments (0));
               Right : constant Long_Float := Evaluate (G.Arguments (1));
            begin
               if Left = Right then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = "=\=" and then Arity = 2 then
            declare
               Left  : constant Long_Float := Evaluate (G.Arguments (0));
               Right : constant Long_Float := Evaluate (G.Arguments (1));
            begin
               if Left /= Right then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = "<" and then Arity = 2 then
            declare
               Left  : constant Long_Float := Evaluate (G.Arguments (0));
               Right : constant Long_Float := Evaluate (G.Arguments (1));
            begin
               if Left < Right then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = ">" and then Arity = 2 then
            declare
               Left  : constant Long_Float := Evaluate (G.Arguments (0));
               Right : constant Long_Float := Evaluate (G.Arguments (1));
            begin
               if Left > Right then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = "=<" and then Arity = 2 then
            declare
               Left  : constant Long_Float := Evaluate (G.Arguments (0));
               Right : constant Long_Float := Evaluate (G.Arguments (1));
            begin
               if Left <= Right then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = ">=" and then Arity = 2 then
            declare
               Left  : constant Long_Float := Evaluate (G.Arguments (0));
               Right : constant Long_Float := Evaluate (G.Arguments (1));
            begin
               if Left >= Right then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         --  Type checking
         if F = "var" and then Arity = 1 then
            if Dereference (G.Arguments (0)).Kind = Term_Variable then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         if F = "nonvar" and then Arity = 1 then
            if Dereference (G.Arguments (0)).Kind /= Term_Variable then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         if F = "atom" and then Arity = 1 then
            if Dereference (G.Arguments (0)).Kind = Term_Atom then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         if F = "integer" and then Arity = 1 then
            if Dereference (G.Arguments (0)).Kind = Term_Integer then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         if F = "number" and then Arity = 1 then
            declare
               T : constant Term_Access := Dereference (G.Arguments (0));
            begin
               if T.Kind = Term_Integer or else T.Kind = Term_Float then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         if F = "float" and then Arity = 1 then
            if Dereference (G.Arguments (0)).Kind = Term_Float then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         if F = "compound" and then Arity = 1 then
            if Dereference (G.Arguments (0)).Kind = Term_Compound then
               return Builtin_Success;
            else
               return Builtin_Failure;
            end if;
         end if;

         --  I/O
         if F = "write" and then Arity = 1 then
            Ada.Text_IO.Put (Term_To_String (Dereference (G.Arguments (0))));
            return Builtin_Success;
         end if;

         if F = "writeln" and then Arity = 1 then
            Ada.Text_IO.Put_Line (Term_To_String (Dereference (G.Arguments (0))));
            return Builtin_Success;
         end if;

         if F = "nl" and then Arity = 0 then
            Ada.Text_IO.New_Line;
            return Builtin_Success;
         end if;

         --  Database
         if F = "asserta" and then Arity = 1 then
            declare
               C : Clause;
               T : constant Term_Access := Dereference (G.Arguments (0));
            begin
               C.Head := Copy_Term (T, Next_Var_Id);
               Next_Var_Id := Next_Var_Id + 1000;
               Assert_A (DB, C);
               return Builtin_Success;
            end;
         end if;

         if (F = "assertz" or else F = "assert") and then Arity = 1 then
            declare
               C : Clause;
               T : constant Term_Access := Dereference (G.Arguments (0));
            begin
               C.Head := Copy_Term (T, Next_Var_Id);
               Next_Var_Id := Next_Var_Id + 1000;
               Assert_Z (DB, C);
               return Builtin_Success;
            end;
         end if;

         --  Term manipulation
         if F = "functor" and then Arity = 3 then
            declare
               T : constant Term_Access := Dereference (G.Arguments (0));
               Name : constant Term_Access := Dereference (G.Arguments (1));
               Ar : constant Term_Access := Dereference (G.Arguments (2));
            begin
               if T.Kind /= Term_Variable then
                  --  Decompose term
                  case T.Kind is
                     when Term_Atom =>
                        if Unify (G.Arguments (1), Make_Atom (To_String (T.Atom_Name)), Tr) and then
                           Unify (G.Arguments (2), Make_Integer (0), Tr) then
                           return Builtin_Success;
                        else
                           return Builtin_Failure;
                        end if;
                     when Term_Integer =>
                        if Unify (G.Arguments (1), Make_Integer (T.Int_Value), Tr) and then
                           Unify (G.Arguments (2), Make_Integer (0), Tr) then
                           return Builtin_Success;
                        else
                           return Builtin_Failure;
                        end if;
                     when Term_Float =>
                        if Unify (G.Arguments (1), Make_Float (T.Float_Value), Tr) and then
                           Unify (G.Arguments (2), Make_Integer (0), Tr) then
                           return Builtin_Success;
                        else
                           return Builtin_Failure;
                        end if;
                     when Term_Compound =>
                        if Unify (G.Arguments (1), Make_Atom (To_String (T.Functor)), Tr) and then
                           Unify (G.Arguments (2), Make_Integer (Natural (T.Arguments.Length)), Tr) then
                           return Builtin_Success;
                        else
                           return Builtin_Failure;
                        end if;
                     when others =>
                        return Builtin_Failure;
                  end case;
               else
                  --  Construct term
                  if Name.Kind = Term_Atom and then Ar.Kind = Term_Integer then
                     if Ar.Int_Value = 0 then
                        if Unify (G.Arguments (0), Make_Atom (To_String (Name.Atom_Name)), Tr) then
                           return Builtin_Success;
                        else
                           return Builtin_Failure;
                        end if;
                     else
                        declare
                           Args : Term_Vectors.Vector;
                        begin
                           for I in 1 .. Ar.Int_Value loop
                              Args.Append (Make_Variable ("_G"));
                           end loop;
                           if Unify (G.Arguments (0), Make_Compound (To_String (Name.Atom_Name), Args), Tr) then
                              return Builtin_Success;
                           else
                              return Builtin_Failure;
                           end if;
                        end;
                     end if;
                  else
                     return Builtin_Failure;
                  end if;
               end if;
            end;
         end if;

         if F = "arg" and then Arity = 3 then
            declare
               N : constant Term_Access := Dereference (G.Arguments (0));
               T : constant Term_Access := Dereference (G.Arguments (1));
            begin
               if N.Kind = Term_Integer and then T.Kind = Term_Compound then
                  if N.Int_Value >= 1 and then N.Int_Value <= Integer (T.Arguments.Length) then
                     if Unify (G.Arguments (2), T.Arguments (N.Int_Value - 1), Tr) then
                        return Builtin_Success;
                     end if;
                  end if;
               end if;
               return Builtin_Failure;
            end;
         end if;

         if F = "copy_term" and then Arity = 2 then
            declare
               Copy : constant Term_Access := Copy_Term (Dereference (G.Arguments (0)), Next_Var_Id);
            begin
               Next_Var_Id := Next_Var_Id + 1000;
               if Unify (G.Arguments (1), Copy, Tr) then
                  return Builtin_Success;
               else
                  return Builtin_Failure;
               end if;
            end;
         end if;

         return Builtin_Not_Builtin;
      end;

   exception
      when Evaluation_Error =>
         return Builtin_Failure;
   end Execute_Builtin;

end Prolog_Builtins;
