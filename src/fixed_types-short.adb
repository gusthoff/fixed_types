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

--  with Ada.Text_IO; use Ada.Text_IO;

package body Fixed_Types.Short is

   overriding
   function "abs" (A : Fixed_Sat_Short) return Fixed_Sat_Short is
   begin
      if A = Fixed_Sat_Short'First then
         return Fixed_Sat_Short'Last;
      else
         return Fixed_Sat_Short (abs Fixed_Short (A));
      end if;
   end "abs";

   overriding
   function "+" (A, B : Fixed_Sat_Short) return Fixed_Sat_Short is
      pragma Suppress (Overflow_Check);
      C    : Fixed_Integer_Short;
      Zero : constant Fixed_Integer_Short := 0;
   begin
      C := To_Fixed_Integer_Short (A) + To_Fixed_Integer_Short (B);

      if A > 0.0 and then B > 0.0 and then C < Zero then
         return Fixed_Sat_Short'Last;
      elsif A < 0.0 and then B < 0.0 and then C > Zero then
         return Fixed_Sat_Short'First;
      else
         return To_Fixed_Sat_Short (C);
      end if;
   end "+";

   overriding
   function "-" (A, B : Fixed_Sat_Short) return Fixed_Sat_Short is
      pragma Suppress (Overflow_Check);
      C    : Fixed_Integer_Short;
      Zero : constant Fixed_Integer_Short := 0;
   begin
      C := To_Fixed_Integer_Short (A) - To_Fixed_Integer_Short (B);

      if A > 0.0 and then B < 0.0 and then C < Zero then
         return Fixed_Sat_Short'Last;
      elsif A < 0.0 and then B > 0.0 and then C > Zero then
         return Fixed_Sat_Short'First;
      else
         return To_Fixed_Sat_Short (C);
      end if;
   end "-";

   overriding
   function "-" (A : Fixed_Sat_Short) return Fixed_Sat_Short is
      pragma Suppress (Overflow_Check);
   begin
      if A = Fixed_Sat_Short'First then
         return Fixed_Sat_Short'Last;
      else
         return Fixed_Sat_Short (-Fixed_Short (A));
      end if;
   end "-";

   not overriding
   function "*" (A, B : Fixed_Sat_Short) return Fixed_Sat_Short is
      pragma Suppress (Overflow_Check);
   begin
      if A = Fixed_Sat_Short'First and then B = Fixed_Sat_Short'First then
         return Fixed_Sat_Short'Last;
      else
         return Fixed_Sat_Short (Fixed_Short (A) * Fixed_Short (B));
      end if;
   end "*";

   overriding
   function "*" (A :  Fixed_Sat_Short; B : Integer) return Fixed_Sat_Short is
      pragma Unsuppress (Overflow_Check);
   begin
      return Fixed_Sat_Short (Fixed_Short (A) * B);
   exception
      when Constraint_Error =>
         if (A > 0.0 and B > 0) or (A < 0.0 and B < 0) then
            return Fixed_Sat_Short'Last;
         else
            return Fixed_Sat_Short'First;
         end if;
   end "*";

end Fixed_Types.Short;

