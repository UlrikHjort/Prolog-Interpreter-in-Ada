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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Prolog_Lexer is

   --  Token types
   type Token_Kind is
     (Tok_Atom,          --  lowercase identifier or quoted atom
      Tok_Variable,      --  uppercase identifier or _
      Tok_Integer,       --  integer literal
      Tok_Float,         --  floating point literal
      Tok_String,        --  "string"
      Tok_Left_Paren,    --  (
      Tok_Right_Paren,   --  )
      Tok_Left_Bracket,  --  [
      Tok_Right_Bracket, --  ]
      Tok_Comma,         --  ,
      Tok_Period,        --  .
      Tok_Pipe,          --  |
      Tok_Implies,       --  :-
      Tok_Query,         --  ?-
      Tok_Cut,           --  !
      Tok_Semicolon,     --  ;
      Tok_Plus,          --  +
      Tok_Minus,         --  -
      Tok_Star,          --  *
      Tok_Slash,         --  /
      Tok_Power,         --  **
      Tok_Arrow,         --  ->
      Tok_DCG_Arrow,     --  -->
      Tok_Equal,         --  =
      Tok_Not_Equal,     --  \=
      Tok_Not,           --  \+
      Tok_Less,          --  <
      Tok_Greater,       --  >
      Tok_Less_Equal,    --  =<
      Tok_Greater_Equal, --  >=
      Tok_Arith_Equal,   --  =:=
      Tok_Arith_Not_Eq,  --  =\=
      Tok_Is,            --  is
      Tok_Mod,           --  mod
      Tok_Unify,         --  =
      Tok_Not_Unify,     --  \=
      Tok_EOF,           --  end of input
      Tok_Error);        --  lexer error

   type Token is record
      Kind  : Token_Kind;
      Value : Unbounded_String;
      Line  : Positive := 1;
      Col   : Positive := 1;
   end record;

   package Token_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Token);

   --  Lexer state
   type Lexer is record
      Input    : Unbounded_String;
      Position : Positive := 1;
      Line     : Positive := 1;
      Col      : Positive := 1;
      Tokens   : Token_Vectors.Vector;
      Current  : Natural := 0;
   end record;

   --  Initialize lexer with input string
   procedure Initialize (L : in out Lexer; Input : String);

   --  Tokenize the entire input
   procedure Tokenize (L : in out Lexer);

   --  Get next token
   function Next_Token (L : in out Lexer) return Token;

   --  Peek at current token without consuming
   function Peek_Token (L : Lexer) return Token;

   --  Check if at end of tokens
   function At_End (L : Lexer) return Boolean;

   --  Reset token position to beginning
   procedure Reset (L : in out Lexer);

end Prolog_Lexer;
