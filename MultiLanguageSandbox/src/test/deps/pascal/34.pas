{$ASSERTIONS ON}

uses
  SysUtils;
(*
You are given an array a of n integers. You can perform at most one operation where 
you select three integers i, j, x (1 <= i <= j <= n) and replace all elements from 
a[i] to a[j] with x, at a cost of (j - i + 1). Find the minimum cost to make all 
elements in the array equal.
    >>> makeEqualAgain(3, [1, 2, 1])
    1
    >>> makeEqualAgain(5, [5, 5, 1, 5, 5])
    1
*)

function makeEqualAgain(n: Integer; a: array of Integer): Integer;
var
  i, left, right: Integer;
begin
  // Check if all elements are the same
  for i := 1 to n - 1 do
  begin
    if a[i] <> a[0] then
      Break;
  end;
  if i = n then
  begin
    makeEqualAgain := 0;
    Exit;
  end;

  // Find the longest prefix with the same value as the first element
  left := 0;
  while (left < n) and (a[left] = a[0]) do
    Inc(left);

  // Find the longest suffix with the same value as the last element
  right := n - 1;
  while (right >= 0) and (a[right] = a[n - 1]) do
    Dec(right);

  // The minimal cost is the length of the middle part that needs to be replaced
  // If the first and last elements are the same, the cost is n - left - (n - 1 - right)
  // Otherwise, it's the smaller of (n - left) or (right + 1)
  if a[0] = a[n - 1] then
    makeEqualAgain := (n - 1 - right) - left + 1
  else
    makeEqualAgain := Min((n - left), (right + 1));
end;
procedure testMakeEqualAgain;
var
  test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, a1, a2, a3, a4, a5, a6: array of Integer;
begin
  test1 := [1, 2, 1];
  test2 := [5, 5, 1, 5, 5];
  test3 := [1, 1, 1, 1];
  test4 := [2, 2, 2, 3, 2, 2];
  test5 := [1];
  test6 := [1, 2];
  test7 := [1, 2, 2, 1];
  test8 := [4, 4, 4, 3, 3, 4, 4];
  test9 := [5, 4, 4, 4, 5, 5];
  test10 := [1, 2, 1, 2, 1, 2, 1];
  a1 := [1, 2, 3, 4, 5, 1];
  a2 := [1, 1, 1, 1, 1, 1, 1];
  a3 := [8, 8, 8, 1, 2, 8, 8, 8];
  a4 := [1, 2, 3];
  a5 := [4, 3, 2, 7, 1, 1, 3];
  a6 := [9, 9, 2, 9, 2, 5, 5, 5, 3];

  Assert(makeEqualAgain(6, a1) = 4);
  Assert(makeEqualAgain(7, a2) = 0);
  Assert(makeEqualAgain(8, a3) = 2);
  Assert(makeEqualAgain(3, a4) = 2);
  Assert(makeEqualAgain(7, a5) = 6);
  Assert(makeEqualAgain(9, a6) = 7);

  Assert(makeEqualAgain(3, test1) = 1);
  Assert(makeEqualAgain(5, test2) = 1);
  Assert(makeEqualAgain(4, test3) = 0);
  Assert(makeEqualAgain(6, test4) = 1);
  Assert(makeEqualAgain(1, test5) = 0);
  Assert(makeEqualAgain(2, test6) = 1);
  Assert(makeEqualAgain(4, test7) = 2);
  Assert(makeEqualAgain(7, test8) = 2);
  Assert(makeEqualAgain(6, test9) = 3);
  Assert(makeEqualAgain(7, test10) = 5);
end;

begin
  testMakeEqualAgain;
end.