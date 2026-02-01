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

with Prolog_Interpreter; use Prolog_Interpreter;

package Prolog_REPL is

   --  Global interpreter instance
   Interp : Interpreter;

   --  Run the interactive REPL
   procedure Run;

   --  Process a single input line (query or directive)
   --  Returns True if should continue, False to exit
   function Process_Input (Input : String) return Boolean;

   --  Print welcome banner
   procedure Print_Banner;

   --  Print help information
   procedure Print_Help;

   --  Load a file into the global interpreter
   procedure Load_File (Filename : String);

   --  Enable query mode (after file is loaded, treat input as queries)
   procedure Enable_Query_Mode;

end Prolog_REPL;
