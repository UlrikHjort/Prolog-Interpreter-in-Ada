-- ***************************************************************************
--          Prolog Parser - Parsing Prolog source code
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

package body Prolog_Parser is

   function Parse_Expression (P : in out Parser; Min_Prec : Natural := 0) return Term_Access;
   function Parse_Primary (P : in out Parser) return Term_Access;
   function Parse_List (P : in out Parser) return Term_Access;
   function Parse_Args (P : in out Parser) return Term_Vectors.Vector;

   function Current (P : Parser) return Token is
   begin
      return Peek_Token (P.Lex);
   end Current;

   procedure Advance (P : in out Parser) is
      Tok : Token;
   begin
      Tok := Next_Token (P.Lex);
   end Advance;

   procedure Expect (P : in out Parser; Kind : Token_Kind) is
   begin
      if Current (P).Kind /= Kind then
         raise Parse_Error with "Expected " & Kind'Image &
                               " but got " & Current (P).Kind'Image;
      end if;
      Advance (P);
   end Expect;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (P : in out Parser; Input : String) is
   begin
      Initialize (P.Lex, Input);
      Tokenize (P.Lex);
      P.Var_Map.Clear;  --  Ensure fresh variable tracking
   end Initialize;

   -------------------
   -- Parse_Primary --
   -------------------

   function Parse_Primary (P : in out Parser) return Term_Access is
      Tok  : constant Token := Current (P);
      Args : Term_Vectors.Vector;
   begin
      case Tok.Kind is
         when Tok_Atom =>
            Advance (P);
            --  Check for compound term
            if Current (P).Kind = Tok_Left_Paren then
               Advance (P);
               Args := Parse_Args (P);
               Expect (P, Tok_Right_Paren);
               return Make_Compound (To_String (Tok.Value), Args);
            else
               return Make_Atom (To_String (Tok.Value));
            end if;

         when Tok_Variable =>
            Advance (P);
            declare
               Var_Name : constant String := To_String (Tok.Value);
            begin
               --  Anonymous variables (starting with _) are always fresh
               if Var_Name'Length > 0 and then Var_Name (Var_Name'First) = '_' then
                  return Make_Variable (Var_Name);
               end if;

               --  Named variables are shared within a clause
               declare
                  Cursor : constant Var_Maps.Cursor := P.Var_Map.Find (Var_Name);
               begin
                  if Var_Maps.Has_Element (Cursor) then
                     --  Reuse existing variable
                     return Var_Maps.Element (Cursor);
                  else
                     --  Create new variable and add to map
                     declare
                        New_Var : constant Term_Access := Make_Variable (Var_Name);
                     begin
                        P.Var_Map.Insert (Var_Name, New_Var);
                        return New_Var;
                     end;
                  end if;
               end;
            end;

         when Tok_Integer =>
            Advance (P);
            return Make_Integer (Integer'Value (To_String (Tok.Value)));

         when Tok_Float =>
            Advance (P);
            return Make_Float (Long_Float'Value (To_String (Tok.Value)));

         when Tok_Left_Paren =>
            Advance (P);
            declare
               Inner : constant Term_Access := Parse_Expression (P);
            begin
               Expect (P, Tok_Right_Paren);
               return Inner;
            end;

         when Tok_Left_Bracket =>
            return Parse_List (P);

         when Tok_Cut =>
            Advance (P);
            return Make_Atom ("!");

         when Tok_Not =>
            --  Negation as failure: \+ Goal
            Advance (P);
            declare
               Goal : constant Term_Access := Parse_Primary (P);
               Not_Args : Term_Vectors.Vector;
            begin
               Not_Args.Append (Goal);
               return Make_Compound ("\\+", Not_Args);
            end;

         when Tok_Minus =>
            Advance (P);
            if Current (P).Kind = Tok_Integer then
               declare
                  Val : constant Token := Current (P);
               begin
                  Advance (P);
                  return Make_Integer (-Integer'Value (To_String (Val.Value)));
               end;
            elsif Current (P).Kind = Tok_Float then
               declare
                  Val : constant Token := Current (P);
               begin
                  Advance (P);
                  return Make_Float (-Long_Float'Value (To_String (Val.Value)));
               end;
            else
               --  Treat as operator
               Args.Append (Make_Integer (0));
               Args.Append (Parse_Primary (P));
               return Make_Compound ("-", Args);
            end if;

         when others =>
            raise Parse_Error with "Unexpected token: " & Tok.Kind'Image;
      end case;
   end Parse_Primary;

   ----------------
   -- Parse_List --
   ----------------

   function Parse_List (P : in out Parser) return Term_Access is
      Elements : Term_Vectors.Vector;
      Tail     : Term_Access;
   begin
      Expect (P, Tok_Left_Bracket);

      if Current (P).Kind = Tok_Right_Bracket then
         Advance (P);
         return Make_Nil;
      end if;

      --  Parse list elements
      loop
         Elements.Append (Parse_Expression (P));
         exit when Current (P).Kind /= Tok_Comma;
         Advance (P);
      end loop;

      --  Check for tail notation [H|T]
      if Current (P).Kind = Tok_Pipe then
         Advance (P);
         Tail := Parse_Expression (P);
      else
         Tail := Make_Nil;
      end if;

      Expect (P, Tok_Right_Bracket);

      --  Build list from elements and tail
      for I in reverse Elements.First_Index .. Elements.Last_Index loop
         Tail := Make_List_Pair (Elements (I), Tail);
      end loop;

      return Tail;
   end Parse_List;

   ----------------
   -- Parse_Args --
   ----------------

   function Parse_Args (P : in out Parser) return Term_Vectors.Vector is
      Args : Term_Vectors.Vector;
   begin
      if Current (P).Kind = Tok_Right_Paren then
         return Args;
      end if;

      loop
         Args.Append (Parse_Expression (P));
         exit when Current (P).Kind /= Tok_Comma;
         Advance (P);
      end loop;

      return Args;
   end Parse_Args;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression (P : in out Parser; Min_Prec : Natural := 0) return Term_Access is
      Left : Term_Access := Parse_Primary (P);
      Op   : Token;
      Right : Term_Access;
      Args : Term_Vectors.Vector;
      Prec : Natural;
      Op_Name : Unbounded_String;
   begin
      loop
         Op := Current (P);

         --  Determine operator precedence
         --  Note: Using inverted Prolog precedences (1200 - prolog_prec) for Pratt parsing
         --  In Prolog, lower number = tighter binding; in Pratt, higher = tighter
         case Op.Kind is
            when Tok_Arrow =>
               Prec := 150;  -- 1200 - 1050 = 150 (tighter than ;)
               Op_Name := To_Unbounded_String ("->");
            when Tok_Semicolon =>
               Prec := 100;  -- 1200 - 1100 = 100 (loosest, binds weakest)
               Op_Name := To_Unbounded_String (";");
            when Tok_Comma =>
               --  Comma in expression context is conjunction
               return Left;
            when Tok_Is =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("is");
            when Tok_Equal | Tok_Unify =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("=");
            when Tok_Not_Equal | Tok_Not_Unify =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("\=");
            when Tok_Arith_Equal =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("=:=");
            when Tok_Arith_Not_Eq =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("=\=");
            when Tok_Less =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("<");
            when Tok_Greater =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String (">");
            when Tok_Less_Equal =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String ("=<");
            when Tok_Greater_Equal =>
               Prec := 500;  -- 1200 - 700 = 500
               Op_Name := To_Unbounded_String (">=");
            when Tok_Plus =>
               Prec := 700;  -- 1200 - 500 = 700
               Op_Name := To_Unbounded_String ("+");
            when Tok_Minus =>
               Prec := 700;  -- 1200 - 500 = 700
               Op_Name := To_Unbounded_String ("-");
            when Tok_Star =>
               Prec := 800;  -- 1200 - 400 = 800 (tighter)
               Op_Name := To_Unbounded_String ("*");
            when Tok_Slash =>
               Prec := 800;  -- 1200 - 400 = 800 (tighter)
               Op_Name := To_Unbounded_String ("/");
            when Tok_Mod =>
               Prec := 800;  -- 1200 - 400 = 800 (same as * and /)
               Op_Name := To_Unbounded_String ("mod");
            when Tok_Power =>
               Prec := 900;  -- 1200 - 200 = 1000 (tighter than * and /)
               Op_Name := To_Unbounded_String ("**");
            when others =>
               return Left;
         end case;

         if Prec < Min_Prec then
            return Left;
         end if;

         Advance (P);
         Right := Parse_Expression (P, Prec);

         Args.Clear;
         Args.Append (Left);
         Args.Append (Right);
         Left := Make_Compound (To_String (Op_Name), Args);
      end loop;
   end Parse_Expression;

   ----------------
   -- Parse_Term --
   ----------------

   function Parse_Term (P : in out Parser) return Term_Access is
   begin
      return Parse_Expression (P);
   end Parse_Term;

   ------------------
   -- Parse_Clause --
   ------------------

   --  DCG transformation helper: add difference list arguments to a term
   function Add_DCG_Args (T : Term_Access; S0, S : Term_Access) return Term_Access is
      New_Args : Term_Vectors.Vector;
   begin
      if T.Kind = Term_Atom then
         --  atom --> atom(S0, S)
         New_Args.Append (S0);
         New_Args.Append (S);
         return Make_Compound (To_String (T.Atom_Name), New_Args);
      elsif T.Kind = Term_Compound then
         --  foo(X) --> foo(X, S0, S)
         for I in T.Arguments.First_Index .. T.Arguments.Last_Index loop
            New_Args.Append (T.Arguments (I));
         end loop;
         New_Args.Append (S0);
         New_Args.Append (S);
         return Make_Compound (To_String (T.Functor), New_Args);
      else
         return T;
      end if;
   end Add_DCG_Args;

   --  DCG transformation: transform a DCG body goal
   function Transform_DCG_Goal (Goal : Term_Access; S_In : in out Term_Access;
                                 S_Out : Term_Access;
                                 Var_Counter : in out Natural) return Term_Access is
   begin
      --  Terminal: [a, b, c] --> S_In = [a, b, c | S_Out]
      if Goal.Kind = Term_Compound and then To_String (Goal.Functor) = "." then
         --  It's a list - unify S_In with [elements | S_Out]
         declare
            Unify_Args : Term_Vectors.Vector;
            List_With_Tail : Term_Access := S_Out;
            Elements : Term_Vectors.Vector;
            Curr : Term_Access := Goal;
         begin
            --  Collect list elements
            while Curr.Kind = Term_Compound and then To_String (Curr.Functor) = "." loop
               Elements.Append (Curr.Arguments (0));
               Curr := Curr.Arguments (1);
            end loop;
            --  Build list with S_Out as tail
            for I in reverse Elements.First_Index .. Elements.Last_Index loop
               List_With_Tail := Make_List_Pair (Elements (I), List_With_Tail);
            end loop;
            Unify_Args.Append (S_In);
            Unify_Args.Append (List_With_Tail);
            S_In := S_Out;  --  For next goal, input is S_Out
            return Make_Compound ("=", Unify_Args);
         end;
      elsif Goal.Kind = Term_Nil then
         --  Empty list [] - just unify S_In = S_Out
         declare
            Unify_Args : Term_Vectors.Vector;
         begin
            Unify_Args.Append (S_In);
            Unify_Args.Append (S_Out);
            S_In := S_Out;
            return Make_Compound ("=", Unify_Args);
         end;
      elsif Goal.Kind = Term_Compound and then To_String (Goal.Functor) = "{}" then
         --  {Goal} - execute Goal without consuming input
         --  S_In stays the same for next goal
         if Natural (Goal.Arguments.Length) = 1 then
            return Goal.Arguments (0);
         else
            return Goal;
         end if;
      else
         --  Non-terminal: foo --> foo(S_In, S_Out)
         declare
            Result : constant Term_Access := Add_DCG_Args (Goal, S_In, S_Out);
         begin
            S_In := S_Out;  --  For next goal, input becomes the output of this goal
            return Result;
         end;
      end if;
   end Transform_DCG_Goal;

   function Parse_Clause (P : in out Parser) return Clause is
      Result : Clause;
      Head   : Term_Access;
   begin
      --  Clear variable map for fresh variable tracking in this clause
      P.Var_Map.Clear;

      Head := Parse_Term (P);
      Result.Head := Head;

      if Current (P).Kind = Tok_Implies then
         Advance (P);
         --  Parse body goals
         loop
            Result.Body_Goals.Append (Parse_Term (P));
            exit when Current (P).Kind /= Tok_Comma;
            Advance (P);
         end loop;
      elsif Current (P).Kind = Tok_DCG_Arrow then
         --  DCG rule: head --> body
         Advance (P);

         --  Create difference list variables
         declare
            S0 : Term_Access := Make_Variable ("_S0");
            S  : constant Term_Access := Make_Variable ("_S");
            S_Current : Term_Access := S0;
            Var_Counter : Natural := 0;
            Body_Goals_Raw : Term_Vectors.Vector;
         begin
            --  Parse DCG body goals
            loop
               Body_Goals_Raw.Append (Parse_Term (P));
               exit when Current (P).Kind /= Tok_Comma;
               Advance (P);
            end loop;

            --  Transform head with difference list args
            Result.Head := Add_DCG_Args (Head, S0, S);

            --  Transform each body goal
            for I in Body_Goals_Raw.First_Index .. Body_Goals_Raw.Last_Index loop
               declare
                  Goal : constant Term_Access := Body_Goals_Raw (I);
                  S_Next : Term_Access;
               begin
                  if I = Body_Goals_Raw.Last_Index then
                     S_Next := S;  --  Last goal outputs to S
                  else
                     Var_Counter := Var_Counter + 1;
                     S_Next := Make_Variable ("_S" & Natural'Image (Var_Counter));
                  end if;
                  Result.Body_Goals.Append (Transform_DCG_Goal (Goal, S_Current, S_Next, Var_Counter));
               end;
            end loop;
         end;
      end if;

      Expect (P, Tok_Period);
      return Result;
   end Parse_Clause;

   -----------------
   -- Parse_Query --
   -----------------

   function Parse_Query (P : in out Parser) return Term_Vectors.Vector is
      Goals : Term_Vectors.Vector;
   begin
      --  Clear variable map for fresh variable tracking in this query
      P.Var_Map.Clear;

      if Current (P).Kind = Tok_Query then
         Advance (P);
      end if;

      loop
         Goals.Append (Parse_Term (P));
         exit when Current (P).Kind /= Tok_Comma;
         Advance (P);
      end loop;

      if Current (P).Kind = Tok_Period then
         Advance (P);
      end if;

      return Goals;
   end Parse_Query;

   -------------------
   -- Parse_Program --
   -------------------

   function Parse_Program (P : in out Parser) return Clause_Vectors.Vector is
      Clauses : Clause_Vectors.Vector;
   begin
      while Has_More (P) loop
         Clauses.Append (Parse_Clause (P));
      end loop;
      return Clauses;
   end Parse_Program;

   --------------
   -- Has_More --
   --------------

   function Has_More (P : Parser) return Boolean is
   begin
      return not At_End (P.Lex) and then Current (P).Kind /= Tok_EOF;
   end Has_More;

end Prolog_Parser;
