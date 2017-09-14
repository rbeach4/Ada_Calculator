-- Ricky Beach
-- ITEC 320
-- Project 4/calc.adb
-- 4/3/2017

with Ada.Text_IO; use Ada.Text_IO;
with calcpkg; use calcpkg;

-- main procedure
procedure calc is
   c : Calculation;
begin
   loop
      get(c);
      exit when length(c) = 0;
      parseInput(c);
      put_line(to_string(c) & result(c)'img);
   end loop;
end calc;
