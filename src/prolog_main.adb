-- ***************************************************************************
--               Prolog Main - Main entry point
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

with Ada.Command_Line;
with Ada.Text_IO;
with Prolog_Interpreter;
with Prolog_REPL;

procedure Prolog_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
begin
   --  Initialize the global interpreter
   Prolog_Interpreter.Initialize (Prolog_REPL.Interp);

   --  Process command line arguments
   for I in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (I);
      begin
         if Arg'Length > 0 and then Arg (Arg'First) /= '-' then
            --  Load Prolog file
            begin
               Prolog_REPL.Load_File (Arg);
               Put_Line ("% " & Arg & " consulted.");
               Prolog_REPL.Enable_Query_Mode;  --  Switch to query mode
            exception
               when others =>
                  Put_Line ("% Error loading " & Arg);
            end;
         elsif Arg = "-h" or else Arg = "--help" then
            Put_Line ("Usage: prolog [options] [files...]");
            Put_Line ("Options:");
            Put_Line ("  -h, --help    Show this help message");
            Put_Line ("  file.pl       Load Prolog file on startup");
            return;
         end if;
      end;
   end loop;

   --  Start REPL
   Prolog_REPL.Run;
end Prolog_Main;
