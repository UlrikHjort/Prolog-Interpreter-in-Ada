-- ***************************************************************************
--          Prolog Interpreter - Query execution engine
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
with Ada.Containers.Ordered_Sets;
with Prolog_Builtins;

package body Prolog_Interpreter is

   --  Choice point for backtracking
   type Choice_Point is record
      Goal            : Term_Access;         --  The goal being tried
      Rest_Goals      : Term_Vectors.Vector; --  Remaining goals after this one
      Clause_Index    : Natural;             --  Next clause to try
      Trail_Position  : Natural;             --  Trail position for undoing
      Var_Id_Position : Natural;             --  Variable ID for fresh vars
   end record;

   package Choice_Point_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Choice_Point);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Interp : in Out Interpreter) is
   begin
      Clear (Interp.DB);
      Clear_Trail (Interp.Tr);
      Interp.Debug := False;
   end Initialize;

   -------------
   -- Consult --
   -------------

   procedure Consult (Interp : in Out Interpreter; Filename : String) is
   begin
      Prolog_Database.Consult (Interp.DB, Filename);
   end Consult;

   ----------------
   -- Add_Clause --
   ----------------

   procedure Add_Clause (Interp : in Out Interpreter; C : Clause) is
   begin
      Assert (Interp.DB, C);
   end Add_Clause;

   ------------------
   -- Solve_Goals  --
   ------------------

   procedure Solve_Goals
     (Interp        : in Out Interpreter;
      Initial_Goals : Term_Vectors.Vector;
      On_Solution   : access procedure;
      Continue      : in Out Boolean)
   is
      Stack          : Choice_Point_Vectors.Vector;
      Current_Goals  : Term_Vectors.Vector := Initial_Goals;
      Cut_Level      : constant Natural := 0;
      Done           : Boolean := False;
      Need_Backtrack : Boolean := False;

      --  Common backtracking procedure - tries next clause from choice points
      procedure Do_Backtrack is
         Backtrack_Done : Boolean := False;
      begin
         Backtrack_Loop :
         loop
            exit Backtrack_Loop when Backtrack_Done;

            if Stack.Is_Empty then
               Done := True;
               Backtrack_Done := True;
            else
               --  Pop choice point and try next clause
               declare
                  CP : constant Choice_Point := Stack.Last_Element;
               begin
                  Stack.Delete_Last;
                  Undo_Bindings (Interp.Tr, CP.Trail_Position);
                  Next_Var_Id := CP.Var_Id_Position;

                  --  Try the next clause for this goal
                  declare
                     Goal    : constant Term_Access := CP.Goal;
                     Key     : constant Predicate_Key := Make_Key (Goal);
                     Clauses : constant Clause_Vectors.Vector := Get_Clauses (Interp.DB, Key);
                     Found   : Boolean := False;
                  begin
                     for I in CP.Clause_Index .. Clauses.Last_Index loop
                        declare
                           C           : constant Clause := Clauses (I);
                           Save_Trail  : constant Natural := Trail_Position (Interp.Tr);
                           Save_Var_Id : constant Natural := Next_Var_Id;
                           Var_Map     : Copy_Entry_Vectors.Vector;
                           Head_Copy   : Term_Access;
                           New_Goals   : Term_Vectors.Vector;
                        begin
                           --  Copy head and body with shared variable map
                           Head_Copy := Copy_Term_With_Map (C.Head, Save_Var_Id, Var_Map);
                           Next_Var_Id := Next_Var_Id + 1000;

                           if Unify (Goal, Head_Copy, Interp.Tr) then
                              Found := True;

                              --  If there are more clauses, save choice point
                              if I < Clauses.Last_Index then
                                 declare
                                    New_CP : Choice_Point;
                                 begin
                                    New_CP.Goal := Goal;
                                    New_CP.Rest_Goals := CP.Rest_Goals;
                                    New_CP.Clause_Index := I + 1;
                                    New_CP.Trail_Position := Save_Trail;
                                    New_CP.Var_Id_Position := Save_Var_Id;
                                    Stack.Append (New_CP);
                                 end;
                              end if;

                              --  Build new goals from clause body + rest
                              for J in C.Body_Goals.First_Index .. C.Body_Goals.Last_Index loop
                                 New_Goals.Append (Copy_Term_With_Map (C.Body_Goals (J), Save_Var_Id, Var_Map));
                              end loop;
                              for J in CP.Rest_Goals.First_Index .. CP.Rest_Goals.Last_Index loop
                                 New_Goals.Append (CP.Rest_Goals (J));
                              end loop;

                              Current_Goals := New_Goals;
                              Backtrack_Done := True;
                              exit;
                           else
                              Undo_Bindings (Interp.Tr, Save_Trail);
                              Next_Var_Id := Save_Var_Id;
                           end if;
                        end;
                     end loop;

                     --  If not found, continue backtracking to next choice point
                     if Found then
                        Backtrack_Done := True;
                     end if;
                  end;
               end;
            end if;
         end loop Backtrack_Loop;
      end Do_Backtrack;

   begin
      Continue := True;

      Main_Loop :
      loop
         exit Main_Loop when Done;

         --  If no goals left, we found a solution
         if Current_Goals.Is_Empty then
            On_Solution.all;
            if not Continue then
               Done := True;
            else
               --  Try to backtrack for more solutions
               Do_Backtrack;
            end if;
         else
            --  Process the current goal
            declare
               Goal        : constant Term_Access := Dereference (Current_Goals.First_Element);
               Rest_Goals  : Term_Vectors.Vector;
               Builtin_Res : Prolog_Builtins.Builtin_Result;
            begin
               --  Build rest of goals
               for I in Current_Goals.First_Index + 1 .. Current_Goals.Last_Index loop
                  Rest_Goals.Append (Current_Goals (I));
               end loop;

               Need_Backtrack := False;

               --  Handle cut
               if Goal.Kind = Term_Atom and then To_String (Goal.Atom_Name) = "!" then
                  while Natural (Stack.Length) > Cut_Level loop
                     Stack.Delete_Last;
                  end loop;
                  Current_Goals := Rest_Goals;

               --  Handle call/1: call(Goal)
               elsif Goal.Kind = Term_Compound and then
                     To_String (Goal.Functor) = "call" and then
                     Natural (Goal.Arguments.Length) = 1
               then
                  --  Execute the goal argument
                  declare
                     Called_Goal : constant Term_Access := Dereference (Goal.Arguments (0));
                     New_Goals   : Term_Vectors.Vector;
                  begin
                     New_Goals.Append (Called_Goal);
                     for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                        New_Goals.Append (Rest_Goals (J));
                     end loop;
                     Current_Goals := New_Goals;
                  end;

               --  Handle if-then-else: (Cond -> Then ; Else) or just disjunction: (A ; B)
               elsif Goal.Kind = Term_Compound and then
                     To_String (Goal.Functor) = ";" and then
                     Natural (Goal.Arguments.Length) = 2
               then
                  declare
                     Left_Arg  : constant Term_Access := Dereference (Goal.Arguments (0));
                     Right_Arg : constant Term_Access := Dereference (Goal.Arguments (1));
                  begin
                     --  Check if left side is an if-then construct: (Cond -> Then)
                     if Left_Arg.Kind = Term_Compound and then
                        To_String (Left_Arg.Functor) = "->" and then
                        Natural (Left_Arg.Arguments.Length) = 2
                     then
                        --  Full if-then-else: (Cond -> Then ; Else)
                        declare
                           Cond_Goal  : constant Term_Access := Left_Arg.Arguments (0);
                           Then_Goal  : constant Term_Access := Left_Arg.Arguments (1);
                           Else_Goal  : constant Term_Access := Right_Arg;
                           Save_Trail : constant Natural := Trail_Position (Interp.Tr);
                           Save_Var_Id : constant Natural := Next_Var_Id;
                           Cond_Result : Boolean := False;
                           Cond_Continue : Boolean := True;

                           procedure On_Cond_Solution is
                           begin
                              Cond_Result := True;
                              Cond_Continue := False;
                           end On_Cond_Solution;

                           Test_Goals : Term_Vectors.Vector;
                        begin
                           Test_Goals.Append (Cond_Goal);
                           --  Save choice point stack size
                           declare
                              Stack_Size : constant Natural := Natural (Stack.Length);
                           begin
                              Solve_Goals (Interp, Test_Goals, On_Cond_Solution'Access, Cond_Continue);
                              if Cond_Result then
                                 --  Condition succeeded - commit and prove Then
                                 --  Remove any choice points created by condition (commit)
                                 while Natural (Stack.Length) > Stack_Size loop
                                    Stack.Delete_Last;
                                 end loop;
                                 declare
                                    New_Goals : Term_Vectors.Vector;
                                 begin
                                    New_Goals.Append (Then_Goal);
                                    for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                                       New_Goals.Append (Rest_Goals (J));
                                    end loop;
                                    Current_Goals := New_Goals;
                                 end;
                              else
                                 --  Condition failed - prove Else
                                 Undo_Bindings (Interp.Tr, Save_Trail);
                                 Next_Var_Id := Save_Var_Id;
                                 declare
                                    New_Goals : Term_Vectors.Vector;
                                 begin
                                    New_Goals.Append (Else_Goal);
                                    for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                                       New_Goals.Append (Rest_Goals (J));
                                    end loop;
                                    Current_Goals := New_Goals;
                                 end;
                              end if;
                           end;
                        end;
                     else
                        --  Simple disjunction: (A ; B) - try A first, if it fails try B
                        --  Save choice point for right alternative
                        declare
                           CP : Choice_Point;
                           Disj_Goals : Term_Vectors.Vector;
                        begin
                           --  Create choice point for trying Right_Arg if Left_Arg fails
                           Disj_Goals.Append (Right_Arg);
                           for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                              Disj_Goals.Append (Rest_Goals (J));
                           end loop;
                           CP.Goal := Right_Arg;
                           CP.Rest_Goals := Rest_Goals;
                           CP.Clause_Index := 0;  -- Not used for disjunction
                           CP.Trail_Position := Trail_Position (Interp.Tr);
                           CP.Var_Id_Position := Next_Var_Id;
                           --  We push a special marker - we'll handle this differently
                           --  Actually, let's just modify Current_Goals to try Left_Arg
                           --  and push the alternative

                           --  For proper backtracking with disjunction, we need to:
                           --  1. Try Left_Arg first
                           --  2. If Left_Arg fails, backtrack will try Right_Arg
                           declare
                              New_Goals : Term_Vectors.Vector;
                           begin
                              New_Goals.Append (Left_Arg);
                              for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                                 New_Goals.Append (Rest_Goals (J));
                              end loop;
                              --  Save choice point for alternative
                              Stack.Append ((Goal => Right_Arg,
                                            Rest_Goals => Rest_Goals,
                                            Clause_Index => 0,
                                            Trail_Position => Trail_Position (Interp.Tr),
                                            Var_Id_Position => Next_Var_Id));
                              Current_Goals := New_Goals;
                           end;
                        end;
                     end if;
                  end;

               --  Handle if-then without else: (Cond -> Then)
               elsif Goal.Kind = Term_Compound and then
                     To_String (Goal.Functor) = "->" and then
                     Natural (Goal.Arguments.Length) = 2
               then
                  --  Simple if-then: if Cond succeeds, prove Then; else fail
                  declare
                     Cond_Goal   : constant Term_Access := Goal.Arguments (0);
                     Then_Goal   : constant Term_Access := Goal.Arguments (1);
                     Save_Trail  : constant Natural := Trail_Position (Interp.Tr);
                     Save_Var_Id : constant Natural := Next_Var_Id;
                     Cond_Result : Boolean := False;
                     Cond_Continue : Boolean := True;

                     procedure On_Cond_Solution is
                     begin
                        Cond_Result := True;
                        Cond_Continue := False;
                     end On_Cond_Solution;

                     Test_Goals : Term_Vectors.Vector;
                  begin
                     Test_Goals.Append (Cond_Goal);
                     declare
                        Stack_Size : constant Natural := Natural (Stack.Length);
                     begin
                        Solve_Goals (Interp, Test_Goals, On_Cond_Solution'Access, Cond_Continue);
                        if Cond_Result then
                           --  Condition succeeded - commit and prove Then
                           while Natural (Stack.Length) > Stack_Size loop
                              Stack.Delete_Last;
                           end loop;
                           declare
                              New_Goals : Term_Vectors.Vector;
                           begin
                              New_Goals.Append (Then_Goal);
                              for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                                 New_Goals.Append (Rest_Goals (J));
                              end loop;
                              Current_Goals := New_Goals;
                           end;
                        else
                           --  Condition failed - backtrack
                           Undo_Bindings (Interp.Tr, Save_Trail);
                           Next_Var_Id := Save_Var_Id;
                           Need_Backtrack := True;
                        end if;
                     end;
                  end;

               --  Handle negation as failure: \+ Goal
               elsif Goal.Kind = Term_Compound and then
                     To_String (Goal.Functor) = "\\+" and then
                     Natural (Goal.Arguments.Length) = 1
               then
                  declare
                     Inner_Goal   : constant Term_Access := Goal.Arguments (0);
                     Test_Goals   : Term_Vectors.Vector;
                     Save_Trail   : constant Natural := Trail_Position (Interp.Tr);
                     Save_Var_Id  : constant Natural := Next_Var_Id;
                     Inner_Result : Boolean := False;
                     Inner_Continue : Boolean := True;

                     procedure On_Inner_Solution is
                     begin
                        Inner_Result := True;
                        Inner_Continue := False;  --  Stop after first solution
                     end On_Inner_Solution;
                  begin
                     Test_Goals.Append (Inner_Goal);
                     --  Don't use Query as it clears the trail; call Solve_Goals directly
                     Solve_Goals (Interp, Test_Goals, On_Inner_Solution'Access, Inner_Continue);
                     --  Undo any bindings made during the test
                     Undo_Bindings (Interp.Tr, Save_Trail);
                     Next_Var_Id := Save_Var_Id;

                     if Inner_Result then
                        --  Inner goal succeeded, so \+ fails
                        Need_Backtrack := True;
                     else
                        --  Inner goal failed, so \+ succeeds
                        Current_Goals := Rest_Goals;
                     end if;
                  end;

               else
                  --  Check if it's a builtin
                  Builtin_Res := Prolog_Builtins.Execute_Builtin (Goal, Interp.Tr, Interp.DB);
                  case Builtin_Res is
                     when Prolog_Builtins.Builtin_Success =>
                        Current_Goals := Rest_Goals;

                     when Prolog_Builtins.Builtin_Failure =>
                        Need_Backtrack := True;

                     when Prolog_Builtins.Builtin_Not_Builtin =>
                        --  Look up clauses in database
                        declare
                           Key     : constant Predicate_Key := Make_Key (Goal);
                           Clauses : constant Clause_Vectors.Vector := Get_Clauses (Interp.DB, Key);
                           Found   : Boolean := False;
                        begin
                           if Clauses.Is_Empty then
                              Need_Backtrack := True;
                           else
                              --  Try each clause
                              for I in Clauses.First_Index .. Clauses.Last_Index loop
                                 declare
                                    C           : constant Clause := Clauses (I);
                                    Save_Trail  : constant Natural := Trail_Position (Interp.Tr);
                                    Save_Var_Id : constant Natural := Next_Var_Id;
                                    Var_Map     : Copy_Entry_Vectors.Vector;
                                    Head_Copy   : Term_Access;
                                    New_Goals   : Term_Vectors.Vector;
                                 begin
                                    --  Copy head and body with shared variable map
                                    Head_Copy := Copy_Term_With_Map (C.Head, Save_Var_Id, Var_Map);
                                    Next_Var_Id := Next_Var_Id + 1000;

                                    if Unify (Goal, Head_Copy, Interp.Tr) then
                                       Found := True;

                                       --  If there are more clauses, save choice point
                                       if I < Clauses.Last_Index then
                                          declare
                                             CP : Choice_Point;
                                          begin
                                             CP.Goal := Goal;
                                             CP.Rest_Goals := Rest_Goals;
                                             CP.Clause_Index := I + 1;
                                             CP.Trail_Position := Save_Trail;
                                             CP.Var_Id_Position := Save_Var_Id;
                                             Stack.Append (CP);
                                          end;
                                       end if;

                                       --  Build new goals from clause body + rest
                                       for J in C.Body_Goals.First_Index .. C.Body_Goals.Last_Index loop
                                          New_Goals.Append (Copy_Term_With_Map (C.Body_Goals (J), Save_Var_Id, Var_Map));
                                       end loop;
                                       for J in Rest_Goals.First_Index .. Rest_Goals.Last_Index loop
                                          New_Goals.Append (Rest_Goals (J));
                                       end loop;

                                       Current_Goals := New_Goals;
                                       exit;
                                    else
                                       Undo_Bindings (Interp.Tr, Save_Trail);
                                       Next_Var_Id := Save_Var_Id;
                                    end if;
                                 end;
                              end loop;

                              if not Found then
                                 Need_Backtrack := True;
                              end if;
                           end if;
                        end;
                  end case;
               end if;

               --  Handle backtracking if needed
               if Need_Backtrack then
                  Do_Backtrack;
               end if;
            end;
         end if;
      end loop Main_Loop;
   end Solve_Goals;

   -----------
   -- Query --
   -----------

   function Query (Interp : in Out Interpreter; Goals : Term_Vectors.Vector) return Boolean is
      Result   : Boolean := False;
      Continue : Boolean := True;

      procedure On_Solution is
      begin
         Result := True;
         Continue := False;  --  Stop after first solution
      end On_Solution;

   begin
      Clear_Trail (Interp.Tr);
      Solve_Goals (Interp, Goals, On_Solution'Access, Continue);
      return Result;
   end Query;

   ---------------
   -- Query_All --
   ---------------

   procedure Query_All
     (Interp   : in Out Interpreter;
      Goals    : Term_Vectors.Vector;
      Callback : Solution_Callback)
   is
      Continue : Boolean := True;

      procedure On_Solution is
      begin
         Continue := Callback.all;
      end On_Solution;

   begin
      Clear_Trail (Interp.Tr);
      Solve_Goals (Interp, Goals, On_Solution'Access, Continue);
   end Query_All;

   -----------------
   -- Get_Binding --
   -----------------

   function Get_Binding (Interp : Interpreter; Var : Term_Access) return Term_Access is
   begin
      return Dereference (Var);
   end Get_Binding;

   --------------------
   -- Print_Bindings --
   --------------------

   procedure Print_Bindings (Interp : Interpreter; Original_Goals : Term_Vectors.Vector) is
      pragma Unreferenced (Interp);
      use Ada.Text_IO;

      package String_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Unbounded_String);

      Seen : String_Sets.Set;

      procedure Collect_Vars (T : Term_Access) is
         D : Term_Access;
      begin
         if T = null then
            return;
         end if;

         D := Dereference (T);

         case T.Kind is
            when Term_Variable =>
               declare
                  Name : constant Unbounded_String := T.Var_Name;
               begin
                  --  Skip anonymous variables
                  if Length (Name) > 0 and then Element (Name, 1) /= '_' then
                     if not Seen.Contains (Name) then
                        Seen.Insert (Name);
                        Put (To_String (Name) & " = " & Term_To_String (D));
                        New_Line;
                     end if;
                  end if;
               end;

            when Term_Compound =>
               for I in T.Arguments.First_Index .. T.Arguments.Last_Index loop
                  Collect_Vars (T.Arguments (I));
               end loop;

            when others =>
               null;
         end case;
      end Collect_Vars;

   begin
      for I in Original_Goals.First_Index .. Original_Goals.Last_Index loop
         Collect_Vars (Original_Goals (I));
      end loop;

      if Seen.Is_Empty then
         Put_Line ("true.");
      end if;
   end Print_Bindings;

end Prolog_Interpreter;
