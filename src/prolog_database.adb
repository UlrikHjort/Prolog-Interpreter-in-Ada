-- ***************************************************************************
--      Prolog Database - Knowledge base for facts and rules
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

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Prolog_Parser;

package body Prolog_Database is

   --------------
   -- Key_Hash --
   --------------

   function Key_Hash (Key : Predicate_Key) return Ada.Containers.Hash_Type is
      use Ada.Containers;
   begin
      return Ada.Strings.Unbounded.Hash (Key.Functor) xor
             Hash_Type (Key.Arity);
   end Key_Hash;

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (Left, Right : Predicate_Key) return Boolean is
   begin
      return Left.Functor = Right.Functor and then Left.Arity = Right.Arity;
   end Key_Equal;

   --------------
   -- Make_Key --
   --------------

   function Make_Key (T : Term_Access) return Predicate_Key is
   begin
      if T = null then
         return (Functor => Null_Unbounded_String, Arity => 0);
      end if;

      case T.Kind is
         when Term_Atom =>
            return (Functor => T.Atom_Name, Arity => 0);
         when Term_Compound =>
            return (Functor => T.Functor, Arity => Natural (T.Arguments.Length));
         when others =>
            return (Functor => Null_Unbounded_String, Arity => 0);
      end case;
   end Make_Key;

   function Make_Key (Functor : String; Arity : Natural) return Predicate_Key is
   begin
      return (Functor => To_Unbounded_String (Functor), Arity => Arity);
   end Make_Key;

   ------------
   -- Assert --
   ------------

   procedure Assert (DB : in out Database; C : Clause) is
   begin
      Assert_Z (DB, C);
   end Assert;

   --------------
   -- Assert_A --
   --------------

   procedure Assert_A (DB : in Out Database; C : Clause) is
      Key : constant Predicate_Key := Make_Key (C.Head);
      Cursor : Clause_Maps.Cursor;
   begin
      Cursor := DB.Clauses.Find (Key);
      if Clause_Maps.Has_Element (Cursor) then
         declare
            Clauses : Clause_Vectors.Vector := Clause_Maps.Element (Cursor);
         begin
            Clauses.Prepend (C);
            DB.Clauses.Replace (Key, Clauses);
         end;
      else
         declare
            Clauses : Clause_Vectors.Vector;
         begin
            Clauses.Append (C);
            DB.Clauses.Insert (Key, Clauses);
         end;
      end if;
   end Assert_A;

   --------------
   -- Assert_Z --
   --------------

   procedure Assert_Z (DB : in Out Database; C : Clause) is
      Key : constant Predicate_Key := Make_Key (C.Head);
      Cursor : Clause_Maps.Cursor;
   begin
      Cursor := DB.Clauses.Find (Key);
      if Clause_Maps.Has_Element (Cursor) then
         declare
            Clauses : Clause_Vectors.Vector := Clause_Maps.Element (Cursor);
         begin
            Clauses.Append (C);
            DB.Clauses.Replace (Key, Clauses);
         end;
      else
         declare
            Clauses : Clause_Vectors.Vector;
         begin
            Clauses.Append (C);
            DB.Clauses.Insert (Key, Clauses);
         end;
      end if;
   end Assert_Z;

   -------------
   -- Retract --
   -------------

   procedure Retract (DB : in Out Database; Head : Term_Access) is
      Key : constant Predicate_Key := Make_Key (Head);
      Cursor : Clause_Maps.Cursor;
   begin
      Cursor := DB.Clauses.Find (Key);
      if Clause_Maps.Has_Element (Cursor) then
         declare
            Clauses : Clause_Vectors.Vector := Clause_Maps.Element (Cursor);
         begin
            if not Clauses.Is_Empty then
               Clauses.Delete_First;
               if Clauses.Is_Empty then
                  DB.Clauses.Delete (Key);
               else
                  DB.Clauses.Replace (Key, Clauses);
               end if;
            end if;
         end;
      end if;
   end Retract;

   -------------
   -- Abolish --
   -------------

   procedure Abolish (DB : in Out Database; Key : Predicate_Key) is
   begin
      if DB.Clauses.Contains (Key) then
         DB.Clauses.Delete (Key);
      end if;
   end Abolish;

   -----------------
   -- Get_Clauses --
   -----------------

   function Get_Clauses (DB : Database; Key : Predicate_Key) return Clause_Vectors.Vector is
      Cursor : constant Clause_Maps.Cursor := DB.Clauses.Find (Key);
   begin
      if Clause_Maps.Has_Element (Cursor) then
         return Clause_Maps.Element (Cursor);
      else
         return Clause_Vectors.Empty_Vector;
      end if;
   end Get_Clauses;

   -------------------
   -- Has_Predicate --
   -------------------

   function Has_Predicate (DB : Database; Key : Predicate_Key) return Boolean is
   begin
      return DB.Clauses.Contains (Key);
   end Has_Predicate;

   -----------
   -- Clear --
   -----------

   procedure Clear (DB : in Out Database) is
   begin
      DB.Clauses.Clear;
   end Clear;

   -------------
   -- Consult --
   -------------

   procedure Consult (DB : in Out Database; Filename : String) is
      use Ada.Text_IO;
      File : File_Type;
      Content : Unbounded_String;
      Line : String (1 .. 4096);
      Last : Natural;
      P : Prolog_Parser.Parser;
      Clauses : Clause_Vectors.Vector;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         Append (Content, Line (1 .. Last));
         Append (Content, ASCII.LF);
      end loop;
      Close (File);

      Prolog_Parser.Initialize (P, To_String (Content));
      Clauses := Prolog_Parser.Parse_Program (P);

      for C of Clauses loop
         Assert (DB, C);
      end loop;
   exception
      when Name_Error =>
         raise Program_Error with "File not found: " & Filename;
   end Consult;

end Prolog_Database;
