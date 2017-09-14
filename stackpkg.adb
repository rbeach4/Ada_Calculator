-- This is the generic implementation for a stack abstract data type.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
package body StackPkg is

   function IsEmpty (s : Stack) return Boolean is
   begin
      return s.Top = 0;
   end IsEmpty;

   function isFull  (s: Stack) return Boolean is
   begin
      return s.Top = Size;
   end isFull;

   procedure Push (Item : ItemType; S : in out Stack) is
   begin
      if IsFull (S) then
         raise Stack_Full;
      else
         S.Top := S.Top + 1;
         S.Elements(S.Top) := Item;
      end if;
   end Push;

   -- Remove an element from Stack s
   procedure pop  (s: in out Stack) is
   begin
      if IsEmpty(s) then
         raise  Stack_Empty;
      else
         s.Top := s.Top - 1;
      end if;
   end pop;

   -- Return top element from Stack s
   function  top   (s: Stack) return ItemType is
   begin
      if IsEmpty(s) then
         raise Stack_Empty;
      else
         return s.Elements(s.Top);
      end if;
   end top;

end StackPkg;
