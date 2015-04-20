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

procedure Fixed_Types_Test is

   procedure Fixed_Test_Long;

   procedure Fixed_Test_Short;

   procedure Fixed_Test_Long is

      package MIO is new Ada.Text_IO.Modular_IO (Modular_Long);

      FL1    :     Fixed_Long;
      FL2    :     Fixed_Long;
      Res_L1 :     Fixed_Long;
      Res_L2 :     Fixed_Long;

      SFL1    :    Fixed_Sat_Long;
      SFL2    :    Fixed_Sat_Long;
      SRes_L1 :    Fixed_Sat_Long;

   begin
      FL1 :=   0.5;
      FL2 :=   0.25;
      for I in 1 .. 10 loop
         Res_L1 := FL1 + FL2;
         Res_L2 := FL1 + FL2;
         --      Put_Line(Integer'Image(Integer(Res_L1)));
         Put_Line ("Res_L1 (fixed):  " & Fixed_Long'Image (Res_L1));
         Put ("Res_L1 (integer): ");
         MIO.Put (Item => Fixed_Long_To_Mod_Long (Res_L1), Base => 16);
         New_Line;

         FL1 := FL1 / 4;
         FL2 := FL2 / 2;
      end loop;
      Put_Line ("Res_L1 (fixed):  " & Fixed_Long'Image (Res_L1));
      Put_Line ("Res_L2 (fixed):  " & Fixed_Long'Image (Res_L2));
      Put_Line ("-----");

      FL1 := 0.5;
      FL2 := 0.5;
      SFL1 := Fixed_Sat_Long (FL1);
      SFL2 := Fixed_Sat_Long (FL2);
      SRes_L1 := SFL1 + SFL2;
      Put_Line ("Res_L1 (fixed):  " & Fixed_Sat_Long'Image (SRes_L1));

      FL1 := -0.5;
      FL2 := -0.5;
      SFL1 := Fixed_Sat_Long (FL1);
      SFL2 := Fixed_Sat_Long (FL2);
      SRes_L1 := SFL1 + SFL2;
      Put_Line ("Res_L1 (fixed):  " & Fixed_Sat_Long'Image (SRes_L1));

      Put_Line ("-----");

      FL1 :=   Fixed_Long'Last;
      FL2 :=   Fixed_Long'Last;

      Put ("FL1 (integer): ");
      MIO.Put (Item => Fixed_Long_To_Mod_Long (FL1), Base => 16);
      New_Line;

      Res_L1 := FL1 + FL2;
      Put_Line ("Res_L1 (fixed):  " & Fixed_Long'Image (Res_L1));
      Put ("Res_L1 (integer): ");
      MIO.Put (Item => Fixed_Long_To_Mod_Long (Res_L1), Base => 16);
      New_Line;

   end Fixed_Test_Long;


   procedure Fixed_Test_Short is

      package MIO is new Ada.Text_IO.Modular_IO (Modular_Short);

      FS1    :     Fixed_Short;
      FS2    :     Fixed_Short;
      Res_S1 :     Fixed_Short;
      Res_S2 :     Fixed_Short;

      SFS1    :    Fixed_Sat_Short;
      SFS2    :    Fixed_Sat_Short;
      SRes_S1 :    Fixed_Sat_Short;

   begin
      FS1 :=   0.5;
      FS2 :=   0.25;
      for I in 1 .. 10 loop
         Res_S1 := FS1 + FS2;
         Res_S2 := FS1 + FS2;
         --      Put_Line(Integer'Image(Integer(Res_S1)));
         Put_Line ("Res_S1 (fixed):  " & Fixed_Short'Image (Res_S1));
         Put ("Res_S1 (integer): ");
         MIO.Put (Item => Fixed_Short_To_Mod_Short (Res_S1), Base => 16);
         New_Line;

         FS1 := FS1 / 4;
         FS2 := FS2 / 2;
      end loop;
      Put_Line ("Res_S1 (fixed):  " & Fixed_Short'Image (Res_S1));
      Put_Line ("Res_S2 (fixed):  " & Fixed_Short'Image (Res_S2));
      Put_Line ("-----");

      FS1 := 0.5;
      FS2 := 0.5;
      SFS1 := Fixed_Sat_Short (FS1);
      SFS2 := Fixed_Sat_Short (FS2);
      SRes_S1 := SFS1 + SFS2;
      Put_Line ("Res_S1 (fixed):  " & Fixed_Sat_Short'Image (SRes_S1));

      FS1 := -0.5;
      FS2 := -0.5;
      SFS1 := Fixed_Sat_Short (FS1);
      SFS2 := Fixed_Sat_Short (FS2);
      SRes_S1 := SFS1 + SFS2;
      Put_Line ("Res_S1 (fixed):  " & Fixed_Sat_Short'Image (SRes_S1));

      Put_Line ("-----");

      FS1 :=   Fixed_Short'Last;
      FS2 :=   Fixed_Short'Last;

      Put ("FS1 (integer): ");
      MIO.Put (Item => Fixed_Short_To_Mod_Short (FS1), Base => 16);
      New_Line;

      Res_S1 := FS1 + FS2;
      Put_Line ("Res_S1 (fixed):  " & Fixed_Short'Image (Res_S1));
      Put ("Res_S1 (integer): ");
      MIO.Put (Item => Fixed_Short_To_Mod_Short (Res_S1), Base => 16);
      New_Line;

   end Fixed_Test_Short;


begin
   Fixed_Test_Long;
   Fixed_Test_Short;

end Fixed_Types_Test;
