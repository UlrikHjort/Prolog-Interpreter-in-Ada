-- ***************************************************************************
--         Prolog Terms - Term representation for Prolog
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

with Ada.Strings.Fixed;

package body Prolog_Terms is

   ---------------
   -- Make_Atom --
   ---------------

   function Make_Atom (Name : String) return Term_Access is
   begin
      return new Term'(Kind      => Term_Atom,
                       Atom_Name => To_Unbounded_String (Name));
   end Make_Atom;

   -------------------
   -- Make_Variable --
   -------------------

   function Make_Variable (Name : String; Id : Natural := 0) return Term_Access is
      Actual_Id : Natural := Id;
   begin
      if Id = 0 then
         Next_Var_Id := Next_Var_Id + 1;
         Actual_Id := Next_Var_Id;
      end if;
      return new Term'(Kind     => Term_Variable,
                       Var_Name => To_Unbounded_String (Name),
                       Var_Id   => Actual_Id,
                       Bound_To => null);
   end Make_Variable;

   ------------------
   -- Make_Integer --
   ------------------

   function Make_Integer (Value : Integer) return Term_Access is
   begin
      return new Term'(Kind      => Term_Integer,
                       Int_Value => Value);
   end Make_Integer;

   ----------------
   -- Make_Float --
   ----------------

   function Make_Float (Value : Long_Float) return Term_Access is
   begin
      return new Term'(Kind        => Term_Float,
                       Float_Value => Value);
   end Make_Float;

   -------------------
   -- Make_Compound --
   -------------------

   function Make_Compound (Functor : String; Args : Term_Vectors.Vector) return Term_Access is
   begin
      return new Term'(Kind      => Term_Compound,
                       Functor   => To_Unbounded_String (Functor),
                       Arguments => Args);
   end Make_Compound;

   --------------
   -- Make_Nil --
   --------------

   function Make_Nil return Term_Access is
   begin
      return new Term'(Kind => Term_Nil);
   end Make_Nil;

   ---------------
   -- Make_List --
   ---------------

   function Make_List (Elements : Term_Vectors.Vector) return Term_Access is
      Result : Term_Access := Make_Nil;
   begin
      --  Build list from back to front
      for I in reverse Elements.First_Index .. Elements.Last_Index loop
         Result := Make_List_Pair (Elements (I), Result);
      end loop;
      return Result;
   end Make_List;

   --------------------
   -- Make_List_Pair --
   --------------------

   function Make_List_Pair (Head, Tail : Term_Access) return Term_Access is
      Args : Term_Vectors.Vector;
   begin
      Args.Append (Head);
      Args.Append (Tail);
      return Make_Compound (".", Args);
   end Make_List_Pair;

   --------------------
   -- Term_To_String --
   --------------------

   function Term_To_String (T : Term_Access) return String is
   begin
      if T = null then
         return "null";
      end if;

      case T.Kind is
         when Term_Atom =>
            return To_String (T.Atom_Name);

         when Term_Variable =>
            if T.Bound_To /= null then
               return Term_To_String (Dereference (T));
            else
               return To_String (T.Var_Name) & "_" &
                      Ada.Strings.Fixed.Trim (T.Var_Id'Image, Ada.Strings.Left);
            end if;

         when Term_Integer =>
            return Ada.Strings.Fixed.Trim (T.Int_Value'Image, Ada.Strings.Left);

         when Term_Float =>
            return Ada.Strings.Fixed.Trim (Long_Float'Image (T.Float_Value), Ada.Strings.Left);

         when Term_Compound =>
            --  Special handling for lists
            if To_String (T.Functor) = "." and then Natural (T.Arguments.Length) = 2 then
               return "[" & List_To_String (T) & "]";
            end if;

            declare
               Result : Unbounded_String := T.Functor & "(";
            begin
               for I in T.Arguments.First_Index .. T.Arguments.Last_Index loop
                  if I > T.Arguments.First_Index then
                     Append (Result, ", ");
                  end if;
                  Append (Result, Term_To_String (T.Arguments (I)));
               end loop;
               Append (Result, ")");
               return To_String (Result);
            end;

         when Term_Nil =>
            return "[]";
      end case;
   end Term_To_String;

   ---------------------
   -- List_To_String  --
   ---------------------

   function List_To_String (T : Term_Access) return String is
      Tail : Term_Access;
   begin
      if T.Kind = Term_Nil then
         return "";
      elsif T.Kind = Term_Compound and then To_String (T.Functor) = "." then
         Tail := Dereference (T.Arguments (1));
         if Tail.Kind = Term_Nil then
            return Term_To_String (T.Arguments (0));
         elsif Tail.Kind = Term_Compound and then To_String (Tail.Functor) = "." then
            return Term_To_String (T.Arguments (0)) & ", " & List_To_String (Tail);
         else
            return Term_To_String (T.Arguments (0)) & "|" & Term_To_String (Tail);
         end if;
      else
         return Term_To_String (T);
      end if;
   end List_To_String;

   -----------------
   -- Dereference --
   -----------------

   function Dereference (T : Term_Access) return Term_Access is
      Current : Term_Access := T;
   begin
      if Current = null then
         return null;
      end if;

      while Current.Kind = Term_Variable and then Current.Bound_To /= null loop
         Current := Current.Bound_To;
      end loop;
      return Current;
   end Dereference;

   ------------------------
   -- Copy_Term_With_Map --
   ------------------------

   function Copy_Term_With_Map
     (T          : Term_Access;
      Var_Offset : Natural;
      Var_Map    : in Out Copy_Entry_Vectors.Vector) return Term_Access is
   begin
      if T = null then
         return null;
      end if;

      case T.Kind is
         when Term_Atom =>
            return Make_Atom (To_String (T.Atom_Name));

         when Term_Variable =>
            if T.Bound_To /= null then
               return Copy_Term_With_Map (T.Bound_To, Var_Offset, Var_Map);
            else
               --  Check if we've already copied this variable instance
               for E of Var_Map loop
                  if E.Original = T then
                     return E.Copy;
                  end if;
               end loop;

               --  Create new variable and remember the mapping
               declare
                  New_Var : constant Term_Access := new Term'(
                     Kind     => Term_Variable,
                     Var_Name => T.Var_Name,
                     Var_Id   => T.Var_Id + Var_Offset,
                     Bound_To => null);
               begin
                  Var_Map.Append ((Original => T, Copy => New_Var));
                  return New_Var;
               end;
            end if;

         when Term_Integer =>
            return Make_Integer (T.Int_Value);

         when Term_Float =>
            return Make_Float (T.Float_Value);

         when Term_Compound =>
            declare
               New_Args : Term_Vectors.Vector;
            begin
               for I in T.Arguments.First_Index .. T.Arguments.Last_Index loop
                  New_Args.Append (Copy_Term_With_Map (T.Arguments (I), Var_Offset, Var_Map));
               end loop;
               return Make_Compound (To_String (T.Functor), New_Args);
            end;

         when Term_Nil =>
            return Make_Nil;
      end case;
   end Copy_Term_With_Map;

   ---------------
   -- Copy_Term --
   ---------------

   function Copy_Term (T : Term_Access; Var_Offset : Natural) return Term_Access is
      Var_Map : Copy_Entry_Vectors.Vector;
   begin
      return Copy_Term_With_Map (T, Var_Offset, Var_Map);
   end Copy_Term;

   -----------
   -- Arity --
   -----------

   function Arity (T : Term_Access) return Natural is
   begin
      if T = null then
         return 0;
      end if;

      case T.Kind is
         when Term_Compound =>
            return Natural (T.Arguments.Length);
         when others =>
            return 0;
      end case;
   end Arity;

end Prolog_Terms;
