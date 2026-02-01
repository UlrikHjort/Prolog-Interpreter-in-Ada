-- ***************************************************************************
--             Prolog REPL - Read-Eval-Print Loop
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
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Prolog_Parser;
with Prolog_Terms; use Prolog_Terms;
with Prolog_Database;

package body Prolog_REPL is

   --  Global state for solution callback
   G_Solution_Count : Natural := 0;
   G_User_Continue  : Boolean := True;
   G_Original_Goals : Term_Vectors.Vector;

   --  Query mode: after consult, default to treating input as queries
   G_Query_Mode : Boolean := False;

   --  Pending input: if user types a query at the "; " prompt, save it here
   G_Pending_Input : Unbounded_String := Null_Unbounded_String;

   function Solution_Callback return Boolean is
      use Ada.Text_IO;
      Response : String (1 .. 1024);
      Last     : Natural;
   begin
      G_Solution_Count := G_Solution_Count + 1;
      Print_Bindings (Interp, G_Original_Goals);

      --  Ask user if they want more solutions
      Put ("; ");
      begin
         Get_Line (Response, Last);
         if Last = 0 then
            --  Empty line means stop
            G_User_Continue := False;
            return False;
         elsif Ada.Characters.Handling.To_Lower (Response (1)) = ';' then
            return True;  --  Continue searching
         else
            --  User typed something else - save it as pending input
            G_User_Continue := False;
            G_Pending_Input := To_Unbounded_String (Response (1 .. Last));
            return False;  --  Stop current query
         end if;
      exception
         when Ada.IO_Exceptions.End_Error =>
            G_User_Continue := False;
            return False;  --  Stop on EOF
      end;
   end Solution_Callback;

   ------------------
   -- Print_Banner --
   ------------------

   procedure Print_Banner is
      use Ada.Text_IO;
   begin
      Put_Line ("Ada Prolog Interpreter v1.0");
      Put_Line ("Type 'help.' for help, 'halt.' to exit.");
      New_Line;
   end Print_Banner;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
      use Ada.Text_IO;
   begin
      Put_Line ("Commands:");
      Put_Line ("  halt.              - Exit the interpreter");
      Put_Line ("  help.              - Show this help");
      Put_Line ("  consult(file).     - Load a Prolog file");
      Put_Line ("  [file].            - Short form of consult(file)");
      Put_Line ("");
      Put_Line ("Queries (use ?- prefix):");
      Put_Line ("  ?- goal.           - Execute a query");
      Put_Line ("  ?- X = 1 + 2.      - Query with variables");
      Put_Line ("");
      Put_Line ("Assertions (no ?- prefix):");
      Put_Line ("  head :- body.      - Add a rule");
      Put_Line ("  fact.              - Add a fact");
      New_Line;
   end Print_Help;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (Filename : String) is
   begin
      Consult (Interp, Filename);
   end Load_File;

   -----------------------
   -- Enable_Query_Mode --
   -----------------------

   procedure Enable_Query_Mode is
   begin
      G_Query_Mode := True;
   end Enable_Query_Mode;

   --  Strip comments from a line (everything after %)
   function Strip_Comment (S : String) return String is
      In_Quote  : Boolean := False;
      In_DQuote : Boolean := False;
   begin
      for I in S'Range loop
         if In_Quote then
            if S (I) = Ada.Characters.Latin_1.Apostrophe then
               In_Quote := False;
            end if;
         elsif In_DQuote then
            if S (I) = '"' then
               In_DQuote := False;
            end if;
         elsif S (I) = Ada.Characters.Latin_1.Apostrophe then
            In_Quote := True;
         elsif S (I) = '"' then
            In_DQuote := True;
         elsif S (I) = '%' then
            return S (S'First .. I - 1);
         end if;
      end loop;
      return S;
   end Strip_Comment;

   --  Check if input is a complete clause (balanced brackets, ends with period)
   function Is_Complete_Clause (S : String) return Boolean is
      Paren_Count   : Integer := 0;
      Bracket_Count : Integer := 0;
      In_Quote      : Boolean := False;
      In_DQuote     : Boolean := False;
      Found_Period  : Boolean := False;
   begin
      for I in S'Range loop
         declare
            C : constant Character := S (I);
         begin
            if In_Quote then
               if C = Ada.Characters.Latin_1.Apostrophe then
                  In_Quote := False;
               end if;
            elsif In_DQuote then
               if C = '"' then
                  In_DQuote := False;
               end if;
            elsif C = Ada.Characters.Latin_1.Apostrophe then
               In_Quote := True;
            elsif C = '"' then
               In_DQuote := True;
            elsif C = '(' then
               Paren_Count := Paren_Count + 1;
            elsif C = ')' then
               Paren_Count := Paren_Count - 1;
            elsif C = '[' then
               Bracket_Count := Bracket_Count + 1;
            elsif C = ']' then
               Bracket_Count := Bracket_Count - 1;
            elsif C = '.' then
               if Paren_Count = 0 and then Bracket_Count = 0 then
                  --  Check this isn't part of a number (digit before and after)
                  if I < S'Last and then Ada.Characters.Handling.Is_Digit (S (I + 1)) then
                     null;  --  Part of number, continue
                  else
                     Found_Period := True;
                  end if;
               end if;
            end if;
         end;
      end loop;

      return Found_Period and then Paren_Count = 0 and then Bracket_Count = 0;
   end Is_Complete_Clause;

   -------------------
   -- Process_Input --
   -------------------

   function Process_Input (Input : String) return Boolean is
      use Ada.Text_IO;
      Trimmed : constant String := Ada.Strings.Fixed.Trim (Input, Ada.Strings.Both);
      P : Prolog_Parser.Parser;
   begin
      if Trimmed = "" then
         return True;
      end if;

      --  Skip comment lines
      if Trimmed (Trimmed'First) = '%' then
         return True;
      end if;

      --  Check for special commands
      if Trimmed = "halt." then
         return False;
      end if;

      if Trimmed = "help." then
         Print_Help;
         return True;
      end if;

      --  Check if it's a query (starts with ?-) or an assertion
      --  Logic:
      --    1. If starts with ?-, it's definitely a query
      --    2. If contains :-, it's a rule to be added
      --    3. Special predicates (consult, listing, [file]) are always queries
      --    4. Otherwise, check if predicate exists - if yes, it's a query; if no, it's a fact
      declare
         Is_Query : Boolean := False;
         Query_Input : Unbounded_String;
         First_Word : Unbounded_String;
         I : Natural;
      begin
         if Trimmed'Length >= 2 and then Trimmed (Trimmed'First .. Trimmed'First + 1) = "?-" then
            --  Explicit ?- prefix, strip it
            Is_Query := True;
            Query_Input := To_Unbounded_String (Trimmed (Trimmed'First + 2 .. Trimmed'Last));
         elsif Ada.Strings.Fixed.Index (Trimmed, ":-") > 0 then
            --  Contains :- so it's a rule to be added
            Is_Query := False;
            Query_Input := To_Unbounded_String (Trimmed);
         else
            Query_Input := To_Unbounded_String (Trimmed);

            --  Check for special built-in predicates that are always queries
            I := Ada.Strings.Fixed.Index (Trimmed, "(");
            if I > 0 then
               First_Word := To_Unbounded_String (Trimmed (Trimmed'First .. I - 1));
            else
               First_Word := To_Unbounded_String (Trimmed);
            end if;

            if To_String (First_Word) = "consult" or else
               To_String (First_Word) = "listing" or else
               To_String (First_Word) = "halt" or else
               To_String (First_Word) = "help" or else
               To_String (First_Word) = "assert" or else
               To_String (First_Word) = "asserta" or else
               To_String (First_Word) = "assertz" or else
               To_String (First_Word) = "retract" or else
               Trimmed (Trimmed'First) = '['
            then
               Is_Query := True;
            elsif G_Query_Mode then
               --  In query mode (after consult), treat as query
               Is_Query := True;
            else
               --  No special predicate, no ?- prefix, not in query mode
               --  Treat as fact to be added
               Is_Query := False;
            end if;
         end if;

         if Is_Query then
            Prolog_Parser.Initialize (P, To_String (Query_Input));
            declare
               Goals : Term_Vectors.Vector;
            begin
               Goals := Prolog_Parser.Parse_Query (P);

               --  Handle special queries
               if Natural (Goals.Length) > 0 then
                  declare
                     First_Goal : constant Term_Access := Goals.First_Element;
                  begin
                     --  consult/1
                     if First_Goal.Kind = Term_Compound and then
                        To_String (First_Goal.Functor) = "consult" and then
                        Natural (First_Goal.Arguments.Length) = 1
                     then
                        declare
                           Arg : constant Term_Access := First_Goal.Arguments (0);
                           Filename : Unbounded_String;
                        begin
                           if Arg.Kind = Term_Atom then
                              Filename := Arg.Atom_Name;
                              Consult (Interp, To_String (Filename));
                              Put_Line ("% " & To_String (Filename) & " consulted.");
                              G_Query_Mode := True;  --  Switch to query mode
                           end if;
                        end;
                        return True;
                     end if;

                     --  [file] short form for consult
                     if First_Goal.Kind = Term_Compound and then
                        To_String (First_Goal.Functor) = "." and then
                        Natural (First_Goal.Arguments.Length) = 2
                     then
                        declare
                           Head : constant Term_Access := Dereference (First_Goal.Arguments (0));
                           Tail : constant Term_Access := Dereference (First_Goal.Arguments (1));
                        begin
                           if Tail.Kind = Term_Nil and then Head.Kind = Term_Atom then
                              Consult (Interp, To_String (Head.Atom_Name));
                              Put_Line ("% " & To_String (Head.Atom_Name) & " consulted.");
                              G_Query_Mode := True;  --  Switch to query mode
                              return True;
                           end if;
                        end;
                     end if;
                  end;
               end if;

               --  Initialize global state for solution callback
               G_Solution_Count := 0;
               G_User_Continue := True;
               G_Original_Goals := Goals;

               --  Execute query
               Query_All (Interp, Goals, Solution_Callback'Access);

               if G_Solution_Count = 0 then
                  Put_Line ("false.");
               elsif G_User_Continue then
                  Put_Line ("false.");  --  No more solutions
               end if;

            exception
               when Prolog_Parser.Parse_Error =>
                  Put_Line ("% Syntax error in query");
            end;

            return True;
         else
            --  It's an assertion (fact or rule)
            Prolog_Parser.Initialize (P, Trimmed);
            declare
               C : constant Clause := Prolog_Parser.Parse_Clause (P);
            begin
               Add_Clause (Interp, C);
               Put_Line ("% Clause added.");
            end;
            return True;
         end if;
      end;

   exception
      when Prolog_Parser.Parse_Error =>
         Put_Line ("% Syntax error");
         return True;
      when others =>
         Put_Line ("% Error processing input");
         return True;
   end Process_Input;

   ---------
   -- Run --
   ---------

   procedure Run is
      use Ada.Text_IO;
      Line          : String (1 .. 4096);
      Last          : Natural;
      Input         : Unbounded_String;
      Continue      : Boolean := True;
      First_Line    : Boolean;
      Got_Input     : Boolean;
   begin
      Print_Banner;

      while Continue loop
         --  Check for pending input from the "; " prompt
         if Length (G_Pending_Input) > 0 then
            Input := G_Pending_Input;
            G_Pending_Input := Null_Unbounded_String;
            Got_Input := True;
            Put ("?- ");  --  Show prompt for context
            Put_Line (To_String (Input));  --  Echo the pending input
            Continue := Process_Input (To_String (Input));
         else
            Put ("?- ");
            Input := Null_Unbounded_String;
            First_Line := True;
            Got_Input := False;

            --  Read input, handling multi-line clauses
            begin
               Read_Loop :
               loop
                  Get_Line (Line, Last);

               declare
                  Line_Str : constant String := Line (1 .. Last);
                  Stripped : constant String := Strip_Comment (Line_Str);
                  Trimmed  : constant String := Ada.Strings.Fixed.Trim (Stripped, Ada.Strings.Both);
               begin
                  --  Skip empty lines and comment-only lines
                  if Trimmed = "" then
                     null;  --  Just continue reading without extra prompt
                  else
                     --  We have actual content
                     Got_Input := True;
                     First_Line := False;

                     --  Append this line's content (with space separator if needed)
                     if Length (Input) > 0 then
                        Append (Input, " ");
                     end if;
                     Append (Input, Trimmed);

                     --  Check if we have a complete clause
                     exit Read_Loop when Is_Complete_Clause (To_String (Input));

                     --  Need more input - show continuation prompt
                     Put ("|    ");
                  end if;
               end;
            end loop Read_Loop;

            --  Process the accumulated input
            if Got_Input then
               Continue := Process_Input (To_String (Input));
            end if;

            exception
               when Ada.IO_Exceptions.End_Error =>
                  --  Process any remaining input before exiting
                  if Length (Input) > 0 then
                     declare
                        Dummy : Boolean;
                     begin
                        Dummy := Process_Input (To_String (Input));
                     end;
                  end if;
                  Continue := False;
            end;
         end if;
      end loop;

      Put_Line ("Bye!");
   end Run;

end Prolog_REPL;
