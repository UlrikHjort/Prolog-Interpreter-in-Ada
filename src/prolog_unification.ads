-- ***************************************************************************
--           Prolog Unification - Unification algorithm
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

with Prolog_Terms; use Prolog_Terms;
with Ada.Containers.Vectors;

package Prolog_Unification is

   --  Binding record for trail (undo stack)
   type Binding is record
      Variable : Term_Access;
      Old_Value : Term_Access;
   end record;

   package Binding_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Binding);

   --  Trail for backtracking
   type Trail is record
      Bindings : Binding_Vectors.Vector;
   end record;

   --  Attempt to unify two terms
   --  Returns True if unification succeeds, False otherwise
   --  Bindings are recorded in the trail for later undoing
   function Unify (T1, T2 : Term_Access; Tr : in out Trail) return Boolean;

   --  Check if a variable occurs in a term (occurs check)
   function Occurs_In (Var : Term_Access; T : Term_Access) return Boolean;

   --  Get current trail position (for backtracking)
   function Trail_Position (Tr : Trail) return Natural;

   --  Undo bindings back to a saved position
   procedure Undo_Bindings (Tr : in out Trail; Position : Natural);

   --  Clear all bindings
   procedure Clear_Trail (Tr : in out Trail);

end Prolog_Unification;
