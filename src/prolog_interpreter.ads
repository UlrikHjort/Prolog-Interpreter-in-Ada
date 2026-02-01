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

with Prolog_Terms;       use Prolog_Terms;
with Prolog_Unification; use Prolog_Unification;
with Prolog_Database;    use Prolog_Database;
with Ada.Containers.Vectors;

package Prolog_Interpreter is

   --  Interpreter state
   type Interpreter is record
      DB    : Database;
      Tr    : Trail;
      Debug : Boolean := False;
   end record;

   --  Solution callback type
   type Solution_Callback is access function return Boolean;

   --  Initialize interpreter
   procedure Initialize (Interp : in out Interpreter);

   --  Load a Prolog file
   procedure Consult (Interp : in Out Interpreter; Filename : String);

   --  Add a clause
   procedure Add_Clause (Interp : in Out Interpreter; C : Clause);

   --  Execute a query and return True if at least one solution exists
   function Query (Interp : in Out Interpreter; Goals : Term_Vectors.Vector) return Boolean;

   --  Execute a query and call callback for each solution
   --  Callback returns True to continue searching, False to stop
   procedure Query_All
     (Interp   : in Out Interpreter;
      Goals    : Term_Vectors.Vector;
      Callback : Solution_Callback);

   --  Get variable bindings from the current solution
   function Get_Binding (Interp : Interpreter; Var : Term_Access) return Term_Access;

   --  Print current variable bindings
   procedure Print_Bindings (Interp : Interpreter; Original_Goals : Term_Vectors.Vector);

end Prolog_Interpreter;
