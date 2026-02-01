-- ***************************************************************************
--            Prolog Lexer - Tokenization for Prolog
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

with Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Prolog_Lexer is

   function Current_Char (L : Lexer) return Character is
   begin
      if L.Position > Length (L.Input) then
         return Ada.Characters.Latin_1.NUL;
      end if;
      return Element (L.Input, L.Position);
   end Current_Char;

   function Peek_Char (L : Lexer; Offset : Positive := 1) return Character is
   begin
      if L.Position + Offset > Length (L.Input) then
         return Ada.Characters.Latin_1.NUL;
      end if;
      return Element (L.Input, L.Position + Offset);
   end Peek_Char;

   procedure Advance (L : in out Lexer) is
   begin
      if Current_Char (L) = Ada.Characters.Latin_1.LF then
         L.Line := L.Line + 1;
         L.Col := 1;
      else
         L.Col := L.Col + 1;
      end if;
      L.Position := L.Position + 1;
   end Advance;

   procedure Skip_Whitespace (L : in out Lexer) is
   begin
      while L.Position <= Length (L.Input) loop
         case Current_Char (L) is
            when ' ' | Ada.Characters.Latin_1.HT |
                 Ada.Characters.Latin_1.CR | Ada.Characters.Latin_1.LF =>
               Advance (L);
            when '%' =>
               --  Line comment
               while L.Position <= Length (L.Input) and then
                     Current_Char (L) /= Ada.Characters.Latin_1.LF loop
                  Advance (L);
               end loop;
            when '/' =>
               if Peek_Char (L) = '*' then
                  --  Block comment
                  Advance (L);
                  Advance (L);
                  while L.Position <= Length (L.Input) loop
                     if Current_Char (L) = '*' and then Peek_Char (L) = '/' then
                        Advance (L);
                        Advance (L);
                        exit;
                     end if;
                     Advance (L);
                  end loop;
               else
                  return;
               end if;
            when others =>
               return;
         end case;
      end loop;
   end Skip_Whitespace;

   function Scan_Token (L : in out Lexer) return Token is
      Start_Line : Positive := L.Line;
      Start_Col  : Positive := L.Col;
      C          : Character;
      Value      : Unbounded_String;
   begin
      Skip_Whitespace (L);

      if L.Position > Length (L.Input) then
         return (Tok_EOF, Null_Unbounded_String, L.Line, L.Col);
      end if;

      Start_Line := L.Line;
      Start_Col := L.Col;
      C := Current_Char (L);

      --  Single character tokens
      case C is
         when '(' =>
            Advance (L);
            return (Tok_Left_Paren, To_Unbounded_String ("("), Start_Line, Start_Col);
         when ')' =>
            Advance (L);
            return (Tok_Right_Paren, To_Unbounded_String (")"), Start_Line, Start_Col);
         when '[' =>
            Advance (L);
            return (Tok_Left_Bracket, To_Unbounded_String ("["), Start_Line, Start_Col);
         when ']' =>
            Advance (L);
            return (Tok_Right_Bracket, To_Unbounded_String ("]"), Start_Line, Start_Col);
         when ',' =>
            Advance (L);
            return (Tok_Comma, To_Unbounded_String (","), Start_Line, Start_Col);
         when '|' =>
            Advance (L);
            return (Tok_Pipe, To_Unbounded_String ("|"), Start_Line, Start_Col);
         when '!' =>
            Advance (L);
            return (Tok_Cut, To_Unbounded_String ("!"), Start_Line, Start_Col);
         when ';' =>
            Advance (L);
            return (Tok_Semicolon, To_Unbounded_String (";"), Start_Line, Start_Col);
         when '+' =>
            Advance (L);
            return (Tok_Plus, To_Unbounded_String ("+"), Start_Line, Start_Col);
         when '*' =>
            Advance (L);
            if Current_Char (L) = '*' then
               Advance (L);
               return (Tok_Power, To_Unbounded_String ("**"), Start_Line, Start_Col);
            end if;
            return (Tok_Star, To_Unbounded_String ("*"), Start_Line, Start_Col);
         when '/' =>
            Advance (L);
            return (Tok_Slash, To_Unbounded_String ("/"), Start_Line, Start_Col);
         when others =>
            null;
      end case;

      --  Multi-character tokens
      if C = '.' then
         Advance (L);
         --  Check if it's end of clause (period followed by whitespace/EOF)
         --  or part of a number (handled in integer parsing below)
         return (Tok_Period, To_Unbounded_String ("."), Start_Line, Start_Col);
      end if;

      if C = ':' and then Peek_Char (L) = '-' then
         Advance (L);
         Advance (L);
         return (Tok_Implies, To_Unbounded_String (":-"), Start_Line, Start_Col);
      end if;

      if C = '?' and then Peek_Char (L) = '-' then
         Advance (L);
         Advance (L);
         return (Tok_Query, To_Unbounded_String ("?-"), Start_Line, Start_Col);
      end if;

      if C = '=' then
         Advance (L);
         if Current_Char (L) = ':' and then Peek_Char (L) = '=' then
            Advance (L);
            Advance (L);
            return (Tok_Arith_Equal, To_Unbounded_String ("=:="), Start_Line, Start_Col);
         elsif Current_Char (L) = Ada.Characters.Latin_1.Reverse_Solidus and then Peek_Char (L) = '=' then
            Advance (L);
            Advance (L);
            return (Tok_Arith_Not_Eq, To_Unbounded_String ("=\="), Start_Line, Start_Col);
         elsif Current_Char (L) = '<' then
            Advance (L);
            return (Tok_Less_Equal, To_Unbounded_String ("=<"), Start_Line, Start_Col);
         else
            return (Tok_Equal, To_Unbounded_String ("="), Start_Line, Start_Col);
         end if;
      end if;

      if C = Ada.Characters.Latin_1.Reverse_Solidus then
         Advance (L);
         if Current_Char (L) = '=' then
            Advance (L);
            return (Tok_Not_Equal, To_Unbounded_String ("\="), Start_Line, Start_Col);
         elsif Current_Char (L) = '+' then
            Advance (L);
            return (Tok_Not, To_Unbounded_String ("\+"), Start_Line, Start_Col);
         end if;
         return (Tok_Error, To_Unbounded_String ("\"), Start_Line, Start_Col);
      end if;

      if C = '<' then
         Advance (L);
         return (Tok_Less, To_Unbounded_String ("<"), Start_Line, Start_Col);
      end if;

      if C = '>' then
         Advance (L);
         if Current_Char (L) = '=' then
            Advance (L);
            return (Tok_Greater_Equal, To_Unbounded_String (">="), Start_Line, Start_Col);
         end if;
         return (Tok_Greater, To_Unbounded_String (">"), Start_Line, Start_Col);
      end if;

      if C = '-' then
         Advance (L);
         --  Check for -> or -->
         if Current_Char (L) = '>' then
            Advance (L);
            return (Tok_Arrow, To_Unbounded_String ("->"), Start_Line, Start_Col);
         elsif Current_Char (L) = '-' and then Peek_Char (L) = '>' then
            Advance (L);
            Advance (L);
            return (Tok_DCG_Arrow, To_Unbounded_String ("-->"), Start_Line, Start_Col);
         end if;
         --  Check for negative number
         if Is_Digit (Current_Char (L)) then
            Append (Value, '-');
            while L.Position <= Length (L.Input) and then Is_Digit (Current_Char (L)) loop
               Append (Value, Current_Char (L));
               Advance (L);
            end loop;
            --  Check for floating point
            if Current_Char (L) = '.' and then Is_Digit (Peek_Char (L)) then
               Append (Value, '.');
               Advance (L);
               while L.Position <= Length (L.Input) and then Is_Digit (Current_Char (L)) loop
                  Append (Value, Current_Char (L));
                  Advance (L);
               end loop;
               --  Check for exponent
               if Current_Char (L) = 'e' or else Current_Char (L) = 'E' then
                  Append (Value, Current_Char (L));
                  Advance (L);
                  if Current_Char (L) = '+' or else Current_Char (L) = '-' then
                     Append (Value, Current_Char (L));
                     Advance (L);
                  end if;
                  while L.Position <= Length (L.Input) and then Is_Digit (Current_Char (L)) loop
                     Append (Value, Current_Char (L));
                     Advance (L);
                  end loop;
               end if;
               return (Tok_Float, Value, Start_Line, Start_Col);
            end if;
            return (Tok_Integer, Value, Start_Line, Start_Col);
         end if;
         return (Tok_Minus, To_Unbounded_String ("-"), Start_Line, Start_Col);
      end if;

      --  Integer or Float
      if Is_Digit (C) then
         while L.Position <= Length (L.Input) and then Is_Digit (Current_Char (L)) loop
            Append (Value, Current_Char (L));
            Advance (L);
         end loop;
         --  Check for floating point: digits followed by '.' and more digits
         if Current_Char (L) = '.' and then Is_Digit (Peek_Char (L)) then
            Append (Value, '.');
            Advance (L);
            while L.Position <= Length (L.Input) and then Is_Digit (Current_Char (L)) loop
               Append (Value, Current_Char (L));
               Advance (L);
            end loop;
            --  Check for exponent (e or E)
            if Current_Char (L) = 'e' or else Current_Char (L) = 'E' then
               Append (Value, Current_Char (L));
               Advance (L);
               if Current_Char (L) = '+' or else Current_Char (L) = '-' then
                  Append (Value, Current_Char (L));
                  Advance (L);
               end if;
               while L.Position <= Length (L.Input) and then Is_Digit (Current_Char (L)) loop
                  Append (Value, Current_Char (L));
                  Advance (L);
               end loop;
            end if;
            return (Tok_Float, Value, Start_Line, Start_Col);
         end if;
         return (Tok_Integer, Value, Start_Line, Start_Col);
      end if;

      --  Variable (starts with uppercase or underscore)
      if Is_Upper (C) or else C = '_' then
         while L.Position <= Length (L.Input) and then
               (Is_Alphanumeric (Current_Char (L)) or else Current_Char (L) = '_') loop
            Append (Value, Current_Char (L));
            Advance (L);
         end loop;
         return (Tok_Variable, Value, Start_Line, Start_Col);
      end if;

      --  Atom (starts with lowercase)
      if Is_Lower (C) then
         while L.Position <= Length (L.Input) and then
               (Is_Alphanumeric (Current_Char (L)) or else Current_Char (L) = '_') loop
            Append (Value, Current_Char (L));
            Advance (L);
         end loop;
         --  Check for keywords
         if To_String (Value) = "is" then
            return (Tok_Is, Value, Start_Line, Start_Col);
         elsif To_String (Value) = "mod" then
            return (Tok_Mod, Value, Start_Line, Start_Col);
         end if;
         return (Tok_Atom, Value, Start_Line, Start_Col);
      end if;

      --  Quoted atom
      if C = Ada.Characters.Latin_1.Apostrophe then
         Advance (L);
         while L.Position <= Length (L.Input) and then Current_Char (L) /= Ada.Characters.Latin_1.Apostrophe loop
            if Current_Char (L) = Ada.Characters.Latin_1.Reverse_Solidus then
               Advance (L);
               if L.Position <= Length (L.Input) then
                  Append (Value, Current_Char (L));
                  Advance (L);
               end if;
            else
               Append (Value, Current_Char (L));
               Advance (L);
            end if;
         end loop;
         if Current_Char (L) = Ada.Characters.Latin_1.Apostrophe then
            Advance (L);
         end if;
         return (Tok_Atom, Value, Start_Line, Start_Col);
      end if;

      --  String
      if C = '"' then
         Advance (L);
         while L.Position <= Length (L.Input) and then Current_Char (L) /= '"' loop
            if Current_Char (L) = Ada.Characters.Latin_1.Reverse_Solidus then
               Advance (L);
               if L.Position <= Length (L.Input) then
                  Append (Value, Current_Char (L));
                  Advance (L);
               end if;
            else
               Append (Value, Current_Char (L));
               Advance (L);
            end if;
         end loop;
         if Current_Char (L) = '"' then
            Advance (L);
         end if;
         return (Tok_String, Value, Start_Line, Start_Col);
      end if;

      --  Unknown character
      Advance (L);
      return (Tok_Error, To_Unbounded_String ((1 => C)), Start_Line, Start_Col);
   end Scan_Token;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (L : in out Lexer; Input : String) is
   begin
      L.Input := To_Unbounded_String (Input);
      L.Position := 1;
      L.Line := 1;
      L.Col := 1;
      L.Tokens.Clear;
      L.Current := 0;
   end Initialize;

   --------------
   -- Tokenize --
   --------------

   procedure Tokenize (L : in out Lexer) is
      Tok : Token;
   begin
      L.Tokens.Clear;
      loop
         Tok := Scan_Token (L);
         L.Tokens.Append (Tok);
         exit when Tok.Kind = Tok_EOF or else Tok.Kind = Tok_Error;
      end loop;
      L.Current := 0;
   end Tokenize;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token (L : in out Lexer) return Token is
   begin
      if L.Current < Natural (L.Tokens.Length) then
         L.Current := L.Current + 1;
         return L.Tokens (L.Current - 1);
      else
         return (Tok_EOF, Null_Unbounded_String, 1, 1);
      end if;
   end Next_Token;

   ----------------
   -- Peek_Token --
   ----------------

   function Peek_Token (L : Lexer) return Token is
   begin
      if L.Current < Natural (L.Tokens.Length) then
         return L.Tokens (L.Current);
      else
         return (Tok_EOF, Null_Unbounded_String, 1, 1);
      end if;
   end Peek_Token;

   ------------
   -- At_End --
   ------------

   function At_End (L : Lexer) return Boolean is
   begin
      return L.Current >= Natural (L.Tokens.Length) or else
             Peek_Token (L).Kind = Tok_EOF;
   end At_End;

   -----------
   -- Reset --
   -----------

   procedure Reset (L : in out Lexer) is
   begin
      L.Current := 0;
   end Reset;

end Prolog_Lexer;
