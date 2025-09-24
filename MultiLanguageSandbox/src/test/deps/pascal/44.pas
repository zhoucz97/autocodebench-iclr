{$ASSERTIONS ON}

uses
  SysUtils, Math;
(*
   Given an array of n distinct integers representing the heights of Kira's friends,
   find the number of ways to choose a triplet (a, b, c) such that the greatest common divisor (GCD)
   of the maximum and minimum values of the triplet is 1.
   Example:
   >>> countTriplets([1, 5, 7])
   1
*)

function gcd(a, b: integer): integer;
begin
  while b <> 0 do
  begin
    a := a mod b;
    if a = 0 then
      a := b;
    b := a mod b;
  end;
  gcd := a;
end;

function countTriplets(heights: array of integer; n: integer): integer;
var
  i, j, k, max_val, min_val: integer;
  count: integer;
begin
  count := 0;
  for i := 0 to n - 1 do
  begin
    for j := i + 1 to n - 1 do
    begin
      for k := j + 1 to n - 1 do
      begin
        // Determine max and min of the triplet
        max_val := heights[i];
        if heights[j] > max_val then
          max_val := heights[j];
        if heights[k] > max_val then
          max_val := heights[k];
        
        min_val := heights[i];
        if heights[j] < min_val then
          min_val := heights[j];
        if heights[k] < min_val then
          min_val := heights[k];
        
        // Check if GCD of max and min is 1
        if gcd(max_val, min_val) = 1 then
          count := count + 1;
      end;
    end;
  end;
  countTriplets := count;
end;
procedure testCountTriplets;
begin
  Assert(countTriplets([1, 5, 7], 3) = 1);
  Assert(countTriplets([1, 6, 2, 3], 4) = 3);
  Assert(countTriplets([16, 4, 8, 2], 4) = 0);
  Assert(countTriplets([10, 1, 6, 7, 9, 8, 4, 3, 5, 2], 10) = 77);
  Assert(countTriplets([4, 5, 9, 11, 14], 5) = 7);
  Assert(countTriplets([15, 10, 6, 9, 7, 3, 12, 8, 11, 1, 2], 11) = 104);
  Assert(countTriplets([3, 7, 11, 13], 4) = 4);
  Assert(countTriplets([5, 12, 13, 17, 19], 5) = 10);
  Assert(countTriplets([2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], 11) = 87);
  Assert(countTriplets([1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17], 11) = 122);
end;

begin
  testCountTriplets;
end.