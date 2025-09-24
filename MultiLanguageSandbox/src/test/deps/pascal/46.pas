{$ASSERTIONS ON}

uses
  SysUtils;
(*
  Given an integer n, this function calculates the sum of all numbers from 1 to n that
  are multiples of either 3 or 5. Each qualifying number is counted only once, even if
  it is a multiple of both 3 and 5.

  Example usage:
    >>> sumOfMultiples(10)
    Returns 33, which represents the sum of 3, 5, 6, 9, and 10.
*)

function sumOfMultiples(n: integer): integer;
var
  i, sum: integer;
begin
  sum := 0;
  for i := 1 to n do
  begin
    if (i mod 3 = 0) or (i mod 5 = 0) then
      sum := sum + i;
  end;
  sumOfMultiples := sum;
end;
procedure testSumOfMultiples;
begin
    Assert(sumOfMultiples(10) = 33);
    Assert(sumOfMultiples(15) = 60);
    Assert(sumOfMultiples(20) = 98);
    Assert(sumOfMultiples(5) = 8);
    Assert(sumOfMultiples(3) = 3);
    Assert(sumOfMultiples(6) = 14);
    Assert(sumOfMultiples(9) = 23);
    Assert(sumOfMultiples(12) = 45);
    Assert(sumOfMultiples(17) = 60);
    Assert(sumOfMultiples(21) = 119);
    Assert(sumOfMultiples(25) = 168);
end;

begin
    testSumOfMultiples;
end.