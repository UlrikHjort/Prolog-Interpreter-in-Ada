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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Prolog_Lexer;  use Prolog_Lexer;
with Prolog_Terms;  use Prolog_Terms;

package Prolog_Parser is

   Parse_Error : exception;

   --  Map from variable name to Term_Access for variable sharing
   package Var_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Term_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Parser is record
      Lex      : Lexer;
      Var_Map  : Var_Maps.Map;  --  Track variables by name during parsing
   end record;

   --  Initialize parser with input string
   procedure Initialize (P : in out Parser; Input : String);

   --  Parse a single term
   function Parse_Term (P : in out Parser) return Term_Access;

   --  Parse a clause (fact or rule)
   function Parse_Clause (P : in out Parser) return Clause;

   --  Parse a query (list of goals)
   function Parse_Query (P : in out Parser) return Term_Vectors.Vector;

   --  Parse entire program (multiple clauses)
   function Parse_Program (P : in out Parser) return Clause_Vectors.Vector;

   --  Check if more input available
   function Has_More (P : Parser) return Boolean;

end Prolog_Parser;
