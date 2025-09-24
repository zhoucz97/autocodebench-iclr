{$ASSERTIONS ON}

uses
  SysUtils;
(*
Given three integers n, m, and k, the task is to find the number of sequences constructed from 
n '(' and m ')', with the longest balanced subsequence having a length of 2 * k. This value 
should be computed modulo 1,000,000,007 (10^9 + 7).

Example usage:
    >>> countBalancedSubsequences(2, 2, 2)
    2
*)

const MOD = 1000000007;

function comb(n, k: int64): int64;
var
  i: int64;
begin
  if k < 0 then
    Exit(0);
  if k > n then
    Exit(0);
  if k = 0 then
    Exit(1);
  if k = 1 then
    Exit(n mod MOD);
  // Precompute factorial, inverse factorial up to n
  // For small n, compute directly
  var res := 1;
  for i := 1 to k do
  begin
    res := res * (n - k + i) mod MOD;
    res := res * inv(i) mod MOD;
  end;
  Exit(res);
end;

// Precompute inverse numbers up to a certain limit for efficiency
// But for the purpose of this problem, we can compute inv on the fly using Fermat's little theorem since MOD is prime.
function inv(x: int64): int64;
begin
  // Compute x^(MOD-2) mod MOD using fast exponentiation
  var res := 1;
  var a := x mod MOD;
  var b := MOD - 2;
  while b > 0 do
  begin
    if (b and 1) = 1 then
      res := res * a mod MOD;
    a := a * a mod MOD;
    b := b shr 1;
  end;
  Exit(res);
end;

function countBalancedSubsequences(n, m, k: int64): int64;
begin
  if (n < k) or (m < k) then
    Exit(0);
  // The valid sequences are those with exactly k '(' and m ')', or exactly n '(' and k ')'
  // But we must ensure that the longest balanced subsequence is exactly 2k.
  // The sequences must not have more than k pairs, which is guaranteed if either:
  // - the number of '(' is exactly k, or
  // - the number of ')' is exactly k.
  var total := 0;
  // Case 1: exactly k '(' and m ')'
  if m >= k then
  begin
    total := (total + comb(n + m - k, k)) mod MOD;
  end;
  // Case 2: exactly n '(' and k ')'
  if n >= k then
  begin
    total := (total + comb(n + m - k, k)) mod MOD;
  end;
  // However, we've double-counted the case where n = k and m = k, so subtract once if both conditions are met.
  if (n = k) and (m = k) then
  begin
    total := (total - 1) mod MOD;
  end;
  Exit(total);
end;
procedure testCountBalancedSubsequences;
begin
  Assert(countBalancedSubsequences(2, 2, 2) = 2);
  Assert(countBalancedSubsequences(3, 2, 3) = 0);
  Assert(countBalancedSubsequences(3, 2, 1) = 4);
  Assert(countBalancedSubsequences(4, 3, 2) = 14);
  Assert(countBalancedSubsequences(5, 5, 2) = 35);
  Assert(countBalancedSubsequences(6, 1, 1) = 6);
  Assert(countBalancedSubsequences(1, 6, 1) = 6);
  Assert(countBalancedSubsequences(7, 2, 2) = 27);
  Assert(countBalancedSubsequences(8, 3, 3) = 110);
  Assert(countBalancedSubsequences(10, 10, 5) = 10659);
  Assert(countBalancedSubsequences(20, 20, 10) = 574221648);
  Assert(countBalancedSubsequences(233, 233, 233) = 280916397);
  Assert(countBalancedSubsequences(233, 666, 233) = 956287259);
  Assert(countBalancedSubsequences(233, 233, 666) = 0);
end;

begin
  // Run the test cases
  testCountBalancedSubsequences;
  WriteLn('All test cases passed');
end.