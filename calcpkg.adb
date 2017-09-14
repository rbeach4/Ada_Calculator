with WordPkg; use WordPkg;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;

package body calcpkg is

   -- converts the words to operators
   function convertWordToOperator (w: WordPkg.word) return operators is
      retOperator: operators;
   begin
      if w = New_Word("(") then
         retOperator := Left_Parenthesis;
      elsif  w = New_Word(")") then
         retOperator := Right_Parenthesis;
      elsif  w = New_Word("**") then
         retOperator := Exponent;
      elsif w = New_Word("*") then
         retOperator := Multiplication_Sign;
      elsif w = New_Word("/") then
         retOperator := Division_Sign;
      elsif  w = New_Word("+") then
         retOperator := Plus_Sign;
      elsif w = New_Word("-") then
         retOperator := Minus_Sign;
      elsif  w = New_Word("=") then
         retOperator := Equals_Sign;
      end if;
      return retOperator;
   end convertWordToOperator;

   -- level of importance for operators
   function assignLevel(operator: operators) return Integer is
      level: Integer;
   begin
      case operator is
         when Minus_Sign => level := 1;
         when Plus_Sign => level := 1;
         when Division_Sign => level := 2;
         when Multiplication_Sign => level := 2;
         when Exponent => level := 3;
         when Left_Parenthesis  => level := 4;
         when Right_Parenthesis => level := 4;
         when Equals_Sign => level := 0;
      end case;
      return level;
   end assignLevel;

   -- got idea from
   -- https://rosettacode.org/wiki/Determine_if_a_string_is_numeric
   function isNum(w: WordPkg.word) return Boolean is
      convertedInt: Integer;
   begin
      convertedInt := Integer'Value(to_string(w));
      return true;
   exception
      when others => return false;
   end isNum;

   -- calculates the result of given input
   function calculateResult(num1, num2: Integer; sign: operators) return
     Integer is
      result: Integer;
   begin
      case sign is
         when Plus_Sign => result := num1 + num2;
         when Minus_Sign => result:= num2 - num1;
         when Multiplication_Sign => result := num2 * num1;
         when Division_Sign => result := num2 / num1;
         when Exponent => result := num2 ** num1;
         when others => raise Non_Operator;
      end case;
      return result;
   exception
      when Constraint_Error => Put_Line("Overflow check has failed");
         GNAT.OS_Lib.OS_Exit(0);
   end calculateResult;

   -- puts items onto stacks and calculates result
   procedure parseInput(c: in out Calculation) is
      convertedOperator: operators;
      assignmentLevel: Integer;
      num1: Integer;
      currentSign: operators;
      num2: Integer;
   begin
      for i in c.arrOfWords'First .. c.arrOfWordsLen loop
         if isNum(c.arrOfWords(i)) and isNum(c.arrOfWords(i + 1)) then
            Put_Line("The calculation entered was not in the correct format");
            GNAT.OS_Lib.OS_Exit(0);
         else
            if isNum(c.arrOfWords(i)) then
               operandPkg.push(Integer'Value(to_string(c.arrOfWords(i))),
                               c.operandStk);
            elsif c.arrOfWords(i) = New_Word("(") then
               convertedOperator := convertWordToOperator(c.arrOfWords(i));
               operatorPkg.push(convertedOperator, c.operatorStk);
            elsif c.arrOfWords(i) = New_Word(")") then
               while operatorPkg.top(c.operatorStk) /= Left_Parenthesis loop
                  num1 := operandPkg.top(c.operandStk);
                  operandPkg.pop(c.operandStk);
                  num2 := operandPkg.top(c.operandStk);
                  operandPkg.pop(c.operandStk);
                  currentSign := operatorPkg.top(c.operatorStk);
                  operatorPkg.pop(c.operatorStk);
                  operandPkg.push(calculateResult(num1, num2, currentSign),
                                  c.operandStk);
               end loop;
               operatorPkg.pop(c.operatorStk);
            elsif c.arrOfWords(i) = New_Word("=") then
               if i = 1 then
                  New_Line;Put_Line("Not a valid calculation");
                  GNAT.OS_Lib.OS_Exit(0);
               else
                  while not operatorPkg.isEmpty(c.operatorStk)  loop
                     num1 := operandPkg.top(c.operandStk);
                     operandPkg.pop(c.operandStk);
                     num2 := operandPkg.top(c.operandStk);
                     operandPkg.pop(c.operandStk);
                     currentSign := operatorPkg.top(c.operatorStk);
                     operatorPkg.pop(c.operatorStk);
                     operandPkg.push(calculateResult(num1, num2, currentSign),
                                     c.operandStk);
                  end loop;

               end if;
            else
               convertedOperator := convertWordToOperator(c.arrOfWords(i));
               assignmentLevel := assignLevel(convertedOperator);
               while (not operatorPkg.isEmpty(c.operatorStk)) and then
                 (assignmentLevel <= assignLevel(operatorPkg.top(c.operatorStk))
                  and assignLevel(operatorPkg.top(c.operatorStk)) /= 4) loop
                  num1 := operandPkg.top(c.operandStk);
                  operandPkg.pop(c.operandStk);
                  num2 := operandPkg.top(c.operandStk);
                  operandPkg.pop(c.operandStk);
                  currentSign := operatorPkg.top(c.operatorStk);
                  operatorPkg.pop(c.operatorStk);
                  operandPkg.push(calculateResult(num1, num2, currentSign),
                                  c.operandStk);
               end loop;
               operatorPkg.push(convertedOperator, c.operatorStk);
            end if;
         end if;
      end loop;
   exception
      when Non_Operator => Put_Line("Non operator encountered");
         GNAT.OS_Lib.OS_Exit(0);
      when operandPkg.Stack_Empty  =>
         Put_Line("Encountered Stack that is empty!");
         GNAT.OS_Lib.OS_Exit(0);
      when operatorPkg.Stack_Empty =>
         Put_Line("Encountered Stack that is empty!");
         GNAT.OS_Lib.OS_Exit(0);

   end parseInput;

   -- converts input to word and adds it to array
   procedure get(c: out Calculation) is
      w: WordPkg.word;
   begin
      c.arrOfWordsLen := 0;
      loop
         get(w);
         if Length(w) = 0 then
            exit;
         end if;
         c.arrOfWordsLen := c.arrOfWordsLen + 1;
         c.arrOfWords(c.arrOfWordsLen) := w;
         exit when c.arrOfWords(c.arrOfWordsLen) = New_Word("=");
      end loop;
   end get;

   procedure put(c: Calculation) is
   begin
      for i in c.arrOfWords'First..c.arrOfWordsLen loop
         put(c.arrOfWords(i));
      end loop;
   end put;

   -- returns the length of a calculation
   function length(c: Calculation) return Natural is
   begin
      return c.arrOfWordsLen;
   end length;

   -- returns the top of the stack for the answer
   function Result(c: Calculation) return Integer is
   begin
      return operandPkg.top(c.operandStk);
   end Result;

   -- prints out the equation to be solved
   function to_string(c: Calculation) return String is
      retStr: Unbounded_String;
   begin
      for i in c.arrOfWords'First..c.arrOfWordsLen loop
         retStr := retStr & to_string(c.arrOfWords(i)) & " ";
      end loop;
      return to_string(retStr);
   end to_string;

end calcpkg;
