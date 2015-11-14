-------------------------------------------------------------------------------
--
--                               FIXED TYPES
--
--                 Fixed_Short & Fixed_Sat_Short definitions
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

package Fixed_Types.Short is

   Fixed_Depth : constant Positive := 16;

   type Fixed_Short is delta 1.0 / 2.0 ** (Fixed_Depth - 1) range -1.0 .. 1.0
     with Size => Fixed_Depth;

   type Fixed_Sat_Short is new Fixed_Short;

   pragma Suppress (Overflow_Check, on => Fixed_Short);
   pragma Suppress (Range_Check, on => Fixed_Short);
--   pragma Suppress (All_checks, on => Fixed_Short);

   type Fixed_Integer_Short is range
     -2**(Fixed_Depth - 1) .. 2**(Fixed_Depth - 1) - 1
     with Size => Fixed_Depth;

   type Modular_Short is mod 2 ** Fixed_Depth with Size => Fixed_Depth;

   function To_Fixed_Integer_Short is new
     Ada.Unchecked_Conversion (Fixed_Short, Fixed_Integer_Short);

   function To_Fixed_Integer_Short is new
     Ada.Unchecked_Conversion (Fixed_Sat_Short, Fixed_Integer_Short);

   function To_Fixed_Short is new
     Ada.Unchecked_Conversion (Fixed_Integer_Short, Fixed_Short);

   function To_Fixed_Sat_Short is new
     Ada.Unchecked_Conversion (Fixed_Integer_Short, Fixed_Sat_Short);

   function Fixed_Short_To_Mod_Short is new
     Ada.Unchecked_Conversion (Fixed_Short, Modular_Short);

   function Fixed_Sat_Short_To_Mod_Short is new
     Ada.Unchecked_Conversion (Fixed_Sat_Short, Modular_Short);

   overriding
   function "abs" (A : Fixed_Sat_Short) return Fixed_Sat_Short;

   overriding
   function "+" (A, B : Fixed_Sat_Short) return Fixed_Sat_Short;

   overriding
   function "-" (A, B : Fixed_Sat_Short) return Fixed_Sat_Short;

   overriding
   function "-" (A : Fixed_Sat_Short) return Fixed_Sat_Short;

   not overriding
   function "*" (A, B : Fixed_Sat_Short) return Fixed_Sat_Short;

   overriding
   function "*" (A :  Fixed_Sat_Short; B : Integer) return Fixed_Sat_Short;

end Fixed_Types.Short;

