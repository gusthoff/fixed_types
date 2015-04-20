-------------------------------------------------------------------------------
--
--                               FIXED TYPES
--
--                  Fixed_Long & Fixed_Sat_Long definitions
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

with Ada.Unchecked_Conversion;

package Fixed_Types.Long is
--
   Fixed_Depth : constant Positive := 32;

   type Fixed_Long is delta 1.0 / 2.0 ** (Fixed_Depth - 1) range -1.0 .. 1.0
     with Size => Fixed_Depth;
   --   for Fixed_Long'Small use 1.0/2.0**(Fixed_Depth-1);

   type Fixed_Sat_Long is new Fixed_Long;

   pragma Suppress (Overflow_Check, on => Fixed_Long);
   pragma Suppress (Range_Check, on => Fixed_Long);
   --      pragma Suppress (All_checks, on => Fixed_Long);

   type Modular_Long is mod 2 ** Fixed_Depth with Size => Fixed_Depth;

   function Fixed_Long_To_Mod_Long is new
     Ada.Unchecked_Conversion (Fixed_Long, Modular_Long);

   function Fixed_Sat_Long_To_Mod_Long is new
     Ada.Unchecked_Conversion (Fixed_Sat_Long, Modular_Long);

   overriding
   function "+" (A, B : Fixed_Sat_Long) return Fixed_Sat_Long;

   overriding
   function "-" (A, B : Fixed_Sat_Long) return Fixed_Sat_Long;

   not overriding
   function "*" (A, B : Fixed_Sat_Long) return Fixed_Sat_Long;

   overriding
   function "*" (A :  Fixed_Sat_Long; B : Integer) return Fixed_Sat_Long;

end Fixed_Types.Long;
