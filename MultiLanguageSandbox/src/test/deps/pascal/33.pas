{$ASSERTIONS ON}
(*
This problem introduces a new modulo operation, denoted by "⊕". Calculation of x ⊕ y:
- If x is not a multiple of y, the result is x mod y.
- Otherwise, if x is a multiple of y, divide x by y until it's no longer a multiple, resulting in x'.
  The result is then x' mod y.
For instance, 4⊕5=4, 20⊕5=4, and 100⊕5=4.
Given a prime number p and an integer n, compute the value of n! ⊕ p, where n! is the factorial of n.
    >>> newModuloFactorial(3, 7)
    6
*)

function newModuloFactorial(n, p: QWord): QWord;
var
  factorial, temp: QWord;
begin
  // Compute n!
  factorial := 1;
  for var i := 2 to n do
  begin
    factorial := factorial * i;
  end;

  // Apply the custom modulo operation ⊕
  temp := factorial;
  while temp mod p = 0 do
  begin
    temp := temp div p;
  end;
  
  Result := temp mod p;
end;
procedure TestNewModuloFactorial;
begin
  Assert(newModuloFactorial(3, 7) = 6);
  Assert(newModuloFactorial(10, 3) = 1);
  Assert(newModuloFactorial(11, 7) = 4);
  Assert(newModuloFactorial(45, 7) = 1);
  Assert(newModuloFactorial(14, 7) = 2);
  Assert(newModuloFactorial(1919, 10007) = 3152);
  Assert(newModuloFactorial(810, 10007) = 3679);
  Assert(newModuloFactorial(3, 7) = 6);
  Assert(newModuloFactorial(10, 3) = 1);
  Assert(newModuloFactorial(1, 2) = 1);
  Assert(newModuloFactorial(5, 11) = 10);
  Assert(newModuloFactorial(6, 13) = 5);
  Assert(newModuloFactorial(8, 17) = 13);
  Assert(newModuloFactorial(15, 19) = 16);
  Assert(newModuloFactorial(21, 23) = 1);
  Assert(newModuloFactorial(30, 29) = 28);
  Assert(newModuloFactorial(100, 101) = 100);
end;

begin
  TestNewModuloFactorial;
  WriteLn('All tests passed successfully.');
end.