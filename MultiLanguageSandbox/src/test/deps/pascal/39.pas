{$ASSERTIONS ON}
(*
Given an array of integers A, the task is to calculate the sum of the XOR of all subarrays.
A subarray is a sequence of consecutive elements of A, defined by two indices L and R,
such that 1 <= L <= R <= n, where n is the size of the array A.
The XOR sum of a subarray is obtained by performing a bitwise XOR operation over all elements from L to R in the subarray.
The final result is the sum of XOR sums for all possible subarrays within the array A.

Example cases:
    >>> sumOfXorSubarrays([1, 2, 3, 4, 5], 5)
    39
*)

function sumOfXorSubarrays(A: array of integer; n: integer): int64;
var
  i, j, xor_val: integer;
  total: int64;
begin
  total := 0;
  for i := 0 to n - 1 do
  begin
    xor_val := 0;
    for j := i to n - 1 do
    begin
      xor_val := xor_val xor A[j];
      total := total + xor_val;
    end;
  end;
  sumOfXorSubarrays := total;
end;
procedure TestSumOfXorSubarrays;
begin
  Assert(sumOfXorSubarrays([1, 2, 3, 4, 5], 5) = 39);
  Assert(sumOfXorSubarrays([1, 1, 1], 3) = 4);
  Assert(sumOfXorSubarrays([2, 3, 1], 3) = 9);
  Assert(sumOfXorSubarrays([4, 5, 7, 9], 4) = 74);
  Assert(sumOfXorSubarrays([0, 0, 0, 0], 4) = 0);
  Assert(sumOfXorSubarrays([8, 8, 8, 8, 8], 5) = 72);
  Assert(sumOfXorSubarrays([3, 6, 9, 12, 15], 5) = 125);
  Assert(sumOfXorSubarrays([10, 20, 30, 40, 50], 5) = 390);
  Assert(sumOfXorSubarrays([16, 16, 16, 16, 16, 16], 6) = 192);
  Assert(sumOfXorSubarrays([1, 3, 5, 7, 9, 11, 13], 7) = 192);
  Assert(sumOfXorSubarrays([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 10) = 218);
end;

begin
  TestSumOfXorSubarrays;
end.