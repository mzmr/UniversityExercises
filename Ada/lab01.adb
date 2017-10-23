with Ada.Text_IO;
use Ada.Text_IO;

procedure Lab01 is

  function Mean(A, B : Float) return Float is
    (A + B) / 2.0;

  function Factorial(X : Integer) return Integer is
    (if X > 1 then Factorial(X - 1) * X else 1);

begin
  Put_Line("Mean = " & Mean(5.6, 6.7)'Img);
  Put_Line("Factorial = " & Factorial(4)'Img);
end Lab01;
