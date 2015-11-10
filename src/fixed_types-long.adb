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

package body Fixed_Types.Long is

   overriding
   function "abs" (A : Fixed_Sat_Long) return Fixed_Sat_Long is
   begin
      if A = Fixed_Sat_Long'First then
         return Fixed_Sat_Long'Last;
      else
         return Fixed_Sat_Long (abs Fixed_Long (A));
      end if;
   end "abs";

   overriding
   function "+" (A, B : Fixed_Sat_Long) return Fixed_Sat_Long is
      pragma Suppress (Overflow_Check);
      C    : Fixed_Integer_Long;
      Zero : constant Fixed_Integer_Long := 0;
   begin
      C := To_Fixed_Integer_Long (A) + To_Fixed_Integer_Long (B);

      if A > 0.0 and then B > 0.0 and then C < Zero then
         return Fixed_Sat_Long'Last;
      elsif A < 0.0 and then B < 0.0 and then C > Zero then
         return Fixed_Sat_Long'First;
      else
         return To_Fixed_Sat_Long (C);
      end if;
   end "+";

   overriding
   function "-" (A, B : Fixed_Sat_Long) return Fixed_Sat_Long is
      pragma Suppress (Overflow_Check);
      C    : Fixed_Integer_Long;
      Zero : constant Fixed_Integer_Long := 0;
   begin
      C := To_Fixed_Integer_Long (A) - To_Fixed_Integer_Long (B);

      if A > 0.0 and then B < 0.0 and then C < Zero then
         return Fixed_Sat_Long'Last;
      elsif A < 0.0 and then B > 0.0 and then C > Zero then
         return Fixed_Sat_Long'First;
      else
         return To_Fixed_Sat_Long (C);
      end if;
   end "-";

   overriding
   function "-" (A : Fixed_Sat_Long) return Fixed_Sat_Long is
      pragma Suppress (Overflow_Check);
   begin
      if A = Fixed_Sat_Long'First then
         return Fixed_Sat_Long'Last;
      else
         return Fixed_Sat_Long (-Fixed_Long (A));
      end if;
   end "-";

   not overriding
   function "*" (A, B : Fixed_Sat_Long) return Fixed_Sat_Long is
      pragma Suppress (Overflow_Check);
   begin
      if A = Fixed_Sat_Long'First and then B = Fixed_Sat_Long'First then
         return Fixed_Sat_Long'Last;
      else
         return Fixed_Sat_Long (Fixed_Long (A) * Fixed_Long (B));
      end if;
   end "*";

   overriding
   function "*" (A :  Fixed_Sat_Long; B : Integer) return Fixed_Sat_Long is
      pragma Unsuppress (Overflow_Check);
   begin
      return Fixed_Sat_Long (Fixed_Long (A) * B);
   exception
      when Constraint_Error =>
         if (A > 0.0 and B > 0) or (A < 0.0 and B < 0) then
            return Fixed_Sat_Long'Last;
         else
            return Fixed_Sat_Long'First;
         end if;
   end "*";

end Fixed_Types.Long;
