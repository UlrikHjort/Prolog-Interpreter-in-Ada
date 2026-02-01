-- ***************************************************************************
--        Prolog Database - Knowledge base for facts and rules
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Prolog_Database is

   --  Predicate indicator: functor/arity
   type Predicate_Key is record
      Functor : Unbounded_String;
      Arity   : Natural;
   end record;

   function Key_Hash (Key : Predicate_Key) return Ada.Containers.Hash_Type;
   function Key_Equal (Left, Right : Predicate_Key) return Boolean;

   --  Map from predicate indicator to clauses
   package Clause_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Predicate_Key,
      Element_Type    => Clause_Vectors.Vector,
      Hash            => Key_Hash,
      Equivalent_Keys => Key_Equal,
      "="             => Clause_Vectors."=");

   --  The database object
   type Database is record
      Clauses : Clause_Maps.Map;
   end record;

   --  Create predicate key from term
   function Make_Key (T : Term_Access) return Predicate_Key;
   function Make_Key (Functor : String; Arity : Natural) return Predicate_Key;

   --  Add a clause to the database
   procedure Assert (DB : in out Database; C : Clause);
   procedure Assert_A (DB : in out Database; C : Clause);  --  Add at beginning
   procedure Assert_Z (DB : in out Database; C : Clause);  --  Add at end

   --  Remove first matching clause
   procedure Retract (DB : in Out Database; Head : Term_Access);

   --  Remove all clauses for a predicate
   procedure Abolish (DB : in out Database; Key : Predicate_Key);

   --  Get all clauses for a predicate
   function Get_Clauses (DB : Database; Key : Predicate_Key) return Clause_Vectors.Vector;

   --  Check if predicate exists
   function Has_Predicate (DB : Database; Key : Predicate_Key) return Boolean;

   --  Clear all clauses
   procedure Clear (DB : in out Database);

   --  Load clauses from a file
   procedure Consult (DB : in out Database; Filename : String);

end Prolog_Database;
