-- ***************************************************************************
--          Prolog Terms - Term representation for Prolog
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

package Prolog_Terms is

   --  Term kinds in Prolog
   type Term_Kind is (Term_Atom, Term_Variable, Term_Integer, Term_Float, Term_Compound, Term_Nil);

   type Term;
   type Term_Access is access all Term;

   --  Vector of terms for compound term arguments
   package Term_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Term_Access);

   --  The main Term type representing all Prolog terms
   type Term (Kind : Term_Kind := Term_Atom) is record
      case Kind is
         when Term_Atom =>
            Atom_Name : Unbounded_String;
         when Term_Variable =>
            Var_Name  : Unbounded_String;
            Var_Id    : Natural;  --  Unique ID for variable renaming
            Bound_To  : Term_Access;  --  Binding (null if unbound)
         when Term_Integer =>
            Int_Value : Integer;
         when Term_Float =>
            Float_Value : Long_Float;
         when Term_Compound =>
            Functor   : Unbounded_String;
            Arguments : Term_Vectors.Vector;
         when Term_Nil =>
            null;
      end case;
   end record;

   --  Clause: Head :- Body_Goals (fact if Body_Goals is empty)
   type Clause is record
      Head       : Term_Access;
      Body_Goals : Term_Vectors.Vector;  --  Goals in the body
   end record;

   package Clause_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Clause);

   --  Constructors
   function Make_Atom (Name : String) return Term_Access;
   function Make_Variable (Name : String; Id : Natural := 0) return Term_Access;
   function Make_Integer (Value : Integer) return Term_Access;
   function Make_Float (Value : Long_Float) return Term_Access;
   function Make_Compound (Functor : String; Args : Term_Vectors.Vector) return Term_Access;
   function Make_Nil return Term_Access;
   function Make_List (Elements : Term_Vectors.Vector) return Term_Access;
   function Make_List_Pair (Head, Tail : Term_Access) return Term_Access;

   --  Utility functions
   function Term_To_String (T : Term_Access) return String;
   function List_To_String (T : Term_Access) return String;
   function Dereference (T : Term_Access) return Term_Access;
   function Copy_Term (T : Term_Access; Var_Offset : Natural) return Term_Access;
   function Arity (T : Term_Access) return Natural;

   --  Variable ID counter for fresh variables
   Next_Var_Id : Natural := 0;

   --  Map for tracking copied variables during Copy_Term
   --  Maps original Term_Access to copied Term_Access
   type Copy_Map_Entry is record
      Original : Term_Access;
      Copy     : Term_Access;
   end record;

   package Copy_Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Copy_Map_Entry);

   --  Copy term with shared variable map (for copying head and body together)
   function Copy_Term_With_Map
     (T          : Term_Access;
      Var_Offset : Natural;
      Var_Map    : in out Copy_Entry_Vectors.Vector) return Term_Access;

end Prolog_Terms;
