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

with Prolog_Terms;       use Prolog_Terms;
with Prolog_Unification; use Prolog_Unification;
with Prolog_Database;    use Prolog_Database;

package Prolog_Builtins is

   --  Builtin result type
   type Builtin_Result is (Builtin_Success, Builtin_Failure, Builtin_Not_Builtin);

   --  Check if a goal is a built-in predicate
   function Is_Builtin (Goal : Term_Access) return Boolean;

   --  Execute a built-in predicate
   --  Returns Builtin_Not_Builtin if not a builtin
   function Execute_Builtin
     (Goal : Term_Access;
      Tr   : in out Trail;
      DB   : in Out Database) return Builtin_Result;

   --  Evaluate arithmetic expression
   function Evaluate (Expr : Term_Access) return Long_Float;

end Prolog_Builtins;
