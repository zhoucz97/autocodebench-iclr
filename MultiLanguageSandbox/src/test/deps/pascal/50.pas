{$ASSERTIONS ON}
(**
Given two integers a and b, return the sum if the sum is even, or return the product of a and b if the sum is odd.
Example usage:
  evenSumOrOddProduct(2, 3) returns 6
*)

function evenSumOrOddProduct(a, b: Integer): Integer;
var
  sum: Integer;
begin
  sum := a + b;
  if (sum mod 2 = 0) then
    evenSumOrOddProduct := sum
  else
    evenSumOrOddProduct := a * b;
end;
procedure testEvenSumOrOddProduct;
begin
  Assert(evenSumOrOddProduct(2, 3) = 6);
  Assert(evenSumOrOddProduct(5, 5) = 10);
  Assert(evenSumOrOddProduct(1, 1) = 2);
  Assert(evenSumOrOddProduct(0, 0) = 0);
  Assert(evenSumOrOddProduct(-1, -1) = -2);
  Assert(evenSumOrOddProduct(100, 200) = 300);
  Assert(evenSumOrOddProduct(3, 4) = 12);
  Assert(evenSumOrOddProduct(-5, 5) = 0);
  Assert(evenSumOrOddProduct(7, 8) = 56);
  Assert(evenSumOrOddProduct(9, 10) = 90);
  Assert(evenSumOrOddProduct(11, 14) = 154);
end;

begin
  testEvenSumOrOddProduct;
end.