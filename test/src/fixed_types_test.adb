-------------------------------------------------------------------------------
--
--                               FIXED TYPES
--
--                             Test application
--
-- The MIT License (MIT)
--
-- Copyright (c) 2015 Gustavo A. Hoffmann
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and /
-- or sell copies of the Software, and to permit persons to whom the Software
-- is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Fixed_Types.Long; use Fixed_Types.Long;
with Fixed_Types.Short; use Fixed_Types.Short;
with Ada.Exceptions;

procedure Fixed_Types_Test is

   procedure Fixed_Test_Long;

   procedure Fixed_Test_Sat_Long;

   procedure Fixed_Test_Short;

   procedure Fixed_Test_Sat_Short;

   procedure Fixed_Test_Long is

      package MIO is new Ada.Text_IO.Modular_IO (Modular_Long);
      package FIO is new Ada.Text_IO.Fixed_IO (Fixed_Long);
      package IIO is new Ada.Text_IO.Integer_IO (Integer);
      FL1    :     Fixed_Long;
      FL2    :     Fixed_Long;
      Res    :     Fixed_Long;

      procedure Print_Operation (A, B, Res : Fixed_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String);

      procedure Print_Operation (A         : Fixed_Long;
                                 B         : Integer;
                                 Res       : Fixed_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String);


      procedure Print_Operation (A, B, Res : Fixed_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         FIO.Put (Item => B,   Fore => 3, Aft  => 3, Exp  => 0);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Long_To_Mod_Long (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         MIO.Put (Item => Fixed_Long_To_Mod_Long (B),
                  Width => 12, Base => 16);
         Put (" = ");
         MIO.Put (Item => Fixed_Long_To_Mod_Long (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;

      procedure Print_Operation (A         : Fixed_Long;
                                 B         : Integer;
                                 Res       : Fixed_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 7);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Long_To_Mod_Long (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 12);
         Put (" = ");
         MIO.Put (Item => Fixed_Long_To_Mod_Long (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;


   begin
      Put_Line ("TEST: Fixed_Test_Long");

      FL1 :=   0.5 / 2**3;
      FL2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FL1 + FL2;

            Print_Operation (FL1, FL2, Res, False, " + ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=   0.25 / 2**3;
      FL2 :=   0.5  / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := FL1 - FL2;

            Print_Operation (FL1, FL2, Res, False, " - ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=  -0.5  / 2**3;
      FL2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := FL1 - FL2;

            Print_Operation (FL1, FL2, Res, False, " - ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=   0.0;
      FL2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := -FL2;

            Print_Operation (FL1, FL2, Res, True, " - ");

            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=   0.0;
      FL2 :=  -0.25 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := abs FL2;

            Print_Operation (FL1, FL2, Res, True, "abs");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=  -0.5  / 2**3;
      FL2 :=  -0.5 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := FL1 * FL2;

            Print_Operation (FL1, FL2, Res, False, " * ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=   0.5 / 2**3;
      FL2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FL1 * FL2;

            Print_Operation (FL1, FL2, Res, False, " * ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 := Fixed_Long'Last;
      FL2 := Fixed_Long'Last;
      Res := FL1 * FL2;
      Print_Operation (FL1, FL2, Res, False, " * ");

      declare
         I1 : Integer := 1;
      begin
         FL1 :=   0.25;
         for I in 1 .. 5 loop
            begin
               Res := FL1 * I1;

               Print_Operation (FL1, I1, Res, False, " * ");
               I1 := I1 + 1;
            exception
               when E : others =>
                  Put_Line ("-- Exception triggered --");
                  Put (Ada.Exceptions.Exception_Information (E));
            end;
         end loop;
      end;

      declare
         I1 : Integer := -1;
      begin
         FL1 :=   0.25;
         for I in 1 .. 5 loop
            begin
               Res := FL1 * I1;

               Print_Operation (FL1, I1, Res, False, " * ");
               I1 := I1 - 1;
            exception
               when E : others =>
                  Put_Line ("-- Exception triggered --");
                  Put (Ada.Exceptions.Exception_Information (E));
            end;
         end loop;
      end;

      FL1 :=   0.25 / 2**3;
      FL2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FL1 / FL2;

            Print_Operation (FL1, FL2, Res, False, " / ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FL1 :=  -0.25 / 2**3;
      FL2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FL1 / FL2;

            Print_Operation (FL1, FL2, Res, False, " / ");

            FL1 := FL1 * 2;
            FL2 := FL2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      Put_Line ("--------------------------------------------------------");

   end Fixed_Test_Long;

   procedure Fixed_Test_Sat_Long is

      package MIO is new Ada.Text_IO.Modular_IO (Modular_Long);
      package FIO is new Ada.Text_IO.Fixed_IO (Fixed_Sat_Long);
      package IIO is new Ada.Text_IO.Integer_IO (Integer);
      SFL1   :     Fixed_Sat_Long;
      SFL2   :     Fixed_Sat_Long;
      SRes   :     Fixed_Sat_Long;

      procedure Print_Operation (A, B, Res : Fixed_Sat_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String);

      procedure Print_Operation (A         : Fixed_Sat_Long;
                                 B         : Integer;
                                 Res       : Fixed_Sat_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String);


      procedure Print_Operation (A, B, Res : Fixed_Sat_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         FIO.Put (Item => B,   Fore => 3, Aft  => 3, Exp  => 0);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Sat_Long_To_Mod_Long (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         MIO.Put (Item => Fixed_Sat_Long_To_Mod_Long (B),
                  Width => 12, Base => 16);
         Put (" = ");
         MIO.Put (Item => Fixed_Sat_Long_To_Mod_Long (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;

      procedure Print_Operation (A         : Fixed_Sat_Long;
                                 B         : Integer;
                                 Res       : Fixed_Sat_Long;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 7);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Sat_Long_To_Mod_Long (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 12);
         Put (" = ");
         MIO.Put (Item => Fixed_Sat_Long_To_Mod_Long (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;

   begin
      Put_Line ("TEST: Fixed_Test_Sat_Long");

      SFL1 :=   0.5 / 2**3;
      SFL2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         SRes := SFL1 + SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " + ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=   0.25 / 2**3;
      SFL2 :=   0.5  / 2**3;
      for I in 0 .. 4 loop
         SRes := SFL1 - SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " - ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=  -0.5  / 2**3;
      SFL2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         SRes := SFL1 - SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " - ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=   0.0;
      SFL2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         SRes := -SFL2;

         Print_Operation (SFL1, SFL2, SRes, True, " - ");

         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=   0.0;
      SFL2 :=  -0.25 / 2**3;
      for I in 0 .. 4 loop
         SRes := abs SFL2;

         Print_Operation (SFL1, SFL2, SRes, True, "abs");

         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=  -0.5  / 2**3;
      SFL2 :=  -0.5 / 2**3;
      for I in 0 .. 4 loop
         SRes := SFL1 * SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " * ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=   0.5 / 2**3;
      SFL2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         SRes := SFL1 * SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " * ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      SFL1 := Fixed_Sat_Long'Last;
      SFL2 := Fixed_Sat_Long'Last;
      SRes := SFL1 * SFL2;
      Print_Operation (SFL1, SFL2, SRes, False, " * ");

      declare
         I1 : Integer := 1;
      begin
         SFL1 :=   0.25;
         for I in 1 .. 5 loop
            SRes := SFL1 * I1;

            Print_Operation (SFL1, I1, SRes, False, " * ");
            I1 := I1 + 1;
         end loop;
      end;

      declare
         I1 : Integer := -1;
      begin
         SFL1 :=   0.25;
         for I in 1 .. 5 loop
            SRes := SFL1 * I1;

            Print_Operation (SFL1, I1, SRes, False, " * ");
            I1 := I1 - 1;
         end loop;
      end;

      SFL1 :=   0.25 / 2**3;
      SFL2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         SRes := SFL1 / SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " / ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      SFL1 :=  -0.25 / 2**3;
      SFL2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         SRes := SFL1 / SFL2;

         Print_Operation (SFL1, SFL2, SRes, False, " / ");

         SFL1 := SFL1 * 2;
         SFL2 := SFL2 * 2;
      end loop;

      Put_Line ("--------------------------------------------------------");
   end Fixed_Test_Sat_Long;

   procedure Fixed_Test_Short is

      package MIO is new Ada.Text_IO.Modular_IO (Modular_Short);
      package FIO is new Ada.Text_IO.Fixed_IO (Fixed_Short);
      package IIO is new Ada.Text_IO.Integer_IO (Integer);
      FS1    :     Fixed_Short;
      FS2    :     Fixed_Short;
      Res    :     Fixed_Short;

      procedure Print_Operation (A, B, Res : Fixed_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String);

      procedure Print_Operation (A         : Fixed_Short;
                                 B         : Integer;
                                 Res       : Fixed_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String);


      procedure Print_Operation (A, B, Res : Fixed_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         FIO.Put (Item => B,   Fore => 3, Aft  => 3, Exp  => 0);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Short_To_Mod_Short (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         MIO.Put (Item => Fixed_Short_To_Mod_Short (B),
                  Width => 12, Base => 16);
         Put (" = ");
         MIO.Put (Item => Fixed_Short_To_Mod_Short (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;

      procedure Print_Operation (A         : Fixed_Short;
                                 B         : Integer;
                                 Res       : Fixed_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 7);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Short_To_Mod_Short (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 12);
         Put (" = ");
         MIO.Put (Item => Fixed_Short_To_Mod_Short (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;


   begin
      Put_Line ("TEST: Fixed_Test_Short");

      FS1 :=   0.5 / 2**3;
      FS2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FS1 + FS2;

            Print_Operation (FS1, FS2, Res, False, " + ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=   0.25 / 2**3;
      FS2 :=   0.5  / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := FS1 - FS2;

            Print_Operation (FS1, FS2, Res, False, " - ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=  -0.5  / 2**3;
      FS2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := FS1 - FS2;

            Print_Operation (FS1, FS2, Res, False, " - ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=   0.0;
      FS2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := -FS2;

            Print_Operation (FS1, FS2, Res, True, " - ");

            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=   0.0;
      FS2 :=  -0.25 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := abs FS2;

            Print_Operation (FS1, FS2, Res, True, "abs");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=  -0.5  / 2**3;
      FS2 :=  -0.5 / 2**3;
      for I in 0 .. 4 loop
         begin
            Res := FS1 * FS2;

            Print_Operation (FS1, FS2, Res, False, " * ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=   0.5 / 2**3;
      FS2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FS1 * FS2;

            Print_Operation (FS1, FS2, Res, False, " * ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 := Fixed_Short'Last;
      FS2 := Fixed_Short'Last;
      Res := FS1 * FS2;
      Print_Operation (FS1, FS2, Res, False, " * ");

      declare
         I1 : Integer := 1;
      begin
         FS1 :=   0.25;
         for I in 1 .. 5 loop
            begin
               Res := FS1 * I1;

               Print_Operation (FS1, I1, Res, False, " * ");
               I1 := I1 + 1;
            exception
               when E : others =>
                  Put_Line ("-- Exception triggered --");
                  Put (Ada.Exceptions.Exception_Information (E));
            end;
         end loop;
      end;

      declare
         I1 : Integer := -1;
      begin
         FS1 :=   0.25;
         for I in 1 .. 5 loop
            begin
               Res := FS1 * I1;

               Print_Operation (FS1, I1, Res, False, " * ");
               I1 := I1 - 1;
            exception
               when E : others =>
                  Put_Line ("-- Exception triggered --");
                  Put (Ada.Exceptions.Exception_Information (E));
            end;
         end loop;
      end;

      FS1 :=   0.25 / 2**3;
      FS2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FS1 / FS2;

            Print_Operation (FS1, FS2, Res, False, " / ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      FS1 :=  -0.25 / 2**3;
      FS2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         begin
            Res := FS1 / FS2;

            Print_Operation (FS1, FS2, Res, False, " / ");

            FS1 := FS1 * 2;
            FS2 := FS2 * 2;
         exception
            when E : others =>
               Put_Line ("-- Exception triggered --");
               Put (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;

      Put_Line ("--------------------------------------------------------");

   end Fixed_Test_Short;

   procedure Fixed_Test_Sat_Short is

      package MIO is new Ada.Text_IO.Modular_IO (Modular_Short);
      package FIO is new Ada.Text_IO.Fixed_IO (Fixed_Sat_Short);
      package IIO is new Ada.Text_IO.Integer_IO (Integer);
      SFS1   :     Fixed_Sat_Short;
      SFS2   :     Fixed_Sat_Short;
      SRes   :     Fixed_Sat_Short;

      procedure Print_Operation (A, B, Res : Fixed_Sat_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String);

      procedure Print_Operation (A         : Fixed_Sat_Short;
                                 B         : Integer;
                                 Res       : Fixed_Sat_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String);


      procedure Print_Operation (A, B, Res : Fixed_Sat_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         FIO.Put (Item => B,   Fore => 3, Aft  => 3, Exp  => 0);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Sat_Short_To_Mod_Short (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         MIO.Put (Item => Fixed_Sat_Short_To_Mod_Short (B),
                  Width => 12, Base => 16);
         Put (" = ");
         MIO.Put (Item => Fixed_Sat_Short_To_Mod_Short (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;

      procedure Print_Operation (A         : Fixed_Sat_Short;
                                 B         : Integer;
                                 Res       : Fixed_Sat_Short;
                                 Unary_Op  : Boolean;
                                 Op        : String) is
      begin
         if Unary_Op then
            Put ("       ");
         else
            FIO.Put (Item => A,   Fore => 3, Aft  => 3, Exp  => 0);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 7);
         Put (" = ");
         FIO.Put (Item => Res, Fore => 3, Aft  => 3, Exp  => 0);
         Put (" (");
         if Unary_Op then
            Put ("            ");
         else
            MIO.Put (Item => Fixed_Sat_Short_To_Mod_Short (A),
                     Width => 12, Base => 16);
         end if;
         Put (" " & Op & " ");
         IIO.Put (Item => B, Width => 12);
         Put (" = ");
         MIO.Put (Item => Fixed_Sat_Short_To_Mod_Short (Res),
                  Width => 12, Base => 16);
         Put (")");
         New_Line;
      end Print_Operation;

   begin
      Put_Line ("TEST: Fixed_Test_Sat_Short");

      SFS1 :=   0.5 / 2**3;
      SFS2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         SRes := SFS1 + SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " + ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=   0.25 / 2**3;
      SFS2 :=   0.5  / 2**3;
      for I in 0 .. 4 loop
         SRes := SFS1 - SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " - ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=  -0.5  / 2**3;
      SFS2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         SRes := SFS1 - SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " - ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=   0.0;
      SFS2 :=   0.25 / 2**3;
      for I in 0 .. 4 loop
         SRes := -SFS2;

         Print_Operation (SFS1, SFS2, SRes, True, " - ");

         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=   0.0;
      SFS2 :=  -0.25 / 2**3;
      for I in 0 .. 4 loop
         SRes := abs SFS2;

         Print_Operation (SFS1, SFS2, SRes, True, "abs");

         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=  -0.5  / 2**3;
      SFS2 :=  -0.5 / 2**3;
      for I in 0 .. 4 loop
         SRes := SFS1 * SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " * ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=   0.5 / 2**3;
      SFS2 :=   0.5 / 2**3;
      for I in 0 .. 3 loop
         SRes := SFS1 * SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " * ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      SFS1 := Fixed_Sat_Short'Last;
      SFS2 := Fixed_Sat_Short'Last;
      SRes := SFS1 * SFS2;
      Print_Operation (SFS1, SFS2, SRes, False, " * ");

      declare
         I1 : Integer := 1;
      begin
         SFS1 :=   0.25;
         for I in 1 .. 5 loop
            SRes := SFS1 * I1;

            Print_Operation (SFS1, I1, SRes, False, " * ");
            I1 := I1 + 1;
         end loop;
      end;

      declare
         I1 : Integer := -1;
      begin
         SFS1 :=   0.25;
         for I in 1 .. 5 loop
            SRes := SFS1 * I1;

            Print_Operation (SFS1, I1, SRes, False, " * ");
            I1 := I1 - 1;
         end loop;
      end;

      SFS1 :=   0.25 / 2**3;
      SFS2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         SRes := SFS1 / SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " / ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      SFS1 :=  -0.25 / 2**3;
      SFS2 :=   0.5  / 2**3;
      for I in 0 .. 3 loop
         SRes := SFS1 / SFS2;

         Print_Operation (SFS1, SFS2, SRes, False, " / ");

         SFS1 := SFS1 * 2;
         SFS2 := SFS2 * 2;
      end loop;

      Put_Line ("--------------------------------------------------------");
   end Fixed_Test_Sat_Short;

begin
   Fixed_Test_Sat_Long;
   Fixed_Test_Long;
   Fixed_Test_Sat_Short;
   Fixed_Test_Short;
end Fixed_Types_Test;
