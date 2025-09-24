{$ASSERTIONS ON}
uses
  SysUtils;
(*
Given a permutation q of n elements and an integer k, find the number of permutations p of n elements such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by dividing p into exactly k non-empty contiguous segments and sorting each segment(that is, choose $ k-1 $ breakpoints $ 1\le x_1 < x_2 < \cdots < x_{k-1} < n $, and divide it into $ [1, x_1], (x_1, x_2], \cdots, (x_{k-1}, n] $). The result should be modulo 998244353.
    >>> countPermutations(2, 1, [1, 2])
    2
*)

const MOD = 998244353;

function countPermutations(n, k: int64; qq: array of int64): int64;
var
  i, j, m, len: int64;
  fact: array of int64;
  segments: array of int64;
begin
  // Precompute factorials up to n modulo MOD
  SetLength(fact, n+1);
  fact[0] := 1;
  for i := 1 to n do
    fact[i] := (fact[i-1] * i) mod MOD;

  // Check if q is already sorted (k=1 case)
  if k = 1 then
  begin
    // All permutations p will have f(p) = sorted p. So if q is sorted, then any p that sorts to q is valid.
    // But since q is a permutation, the only p that sorts to q is q itself (because f(p) is the lex smallest after sorting segments, and for k=1, the whole p is sorted to q).
    // Wait, no. For k=1, f(p) is the sorted version of p. So f(p) = q implies that the sorted version of p is q. So p must be any permutation that sorts to q. But since q is a permutation, the only p that sorts to q is q itself (because sorting p gives q, and p must be a permutation of 1..n, so p must be q). But the sample input has countPermutations(2,1,[1,2]) returning 2. So this suggests that for k=1, any p that is already q (since sorted p is q) is valid. But [1,2] sorted is [1,2], so p can be [1,2] or [2,1]? No, because sorted [2,1] is [1,2], so f([2,1]) = [1,2]. So for k=1, any p that when sorted gives q is valid. So the count is the number of p's that sort to q. Which is 1 if q is already sorted (like [1,2]), because only [1,2] sorts to [1,2]. But the sample returns 2. So this approach is incorrect.
    // Re-evaluating: for k=1, f(p) is the sorted version of p. So f(p) = q means that the sorted version of p is q. So p must be a permutation that when sorted gives q. The number of such p's is the number of permutations of the multiset that q represents when considered as a multiset. But q is a permutation of 1..n, so all elements are distinct. So the only p that sorts to q is q itself. But the sample input expects 2. So this approach is wrong.
    // Alternative interpretation: perhaps for k=1, any p is valid because f(p) is the sorted version of p, and q is the target. So if q is the sorted version of p, then p can be any permutation that sorts to q. For q = [1,2], the permutations that sort to [1,2] are [1,2] and [2,1]. So count is 2. Hence, for k=1, the answer is the factorial of the length of q (since any permutation of q's elements will sort to q). But for q = [1,2], factorial(2) is 2, which matches the sample. So for k=1, the answer is n (since any permutation of 1..n will sort to the sorted version of 1..n, which is [1,2,...,n]). But the sample q is [1,2], which is sorted, so any p that sorts to [1,2] is valid. The number of such p's is 2 (for n=2). So the answer is 2. Hence, for k=1, the answer is n mod MOD. But for the sample, n=2, 2 is 2, which matches. So this seems correct.
    // So for k=1, the answer is n mod MOD.
    Result := fact[n];
    Exit;
  end;

  // For k > 1, we need to split q into k increasing segments. The segments must be strictly increasing within each segment, and the entire q must be the concatenation of these segments in order.
  // Then, the number of valid p's is the product of the factorials of the lengths of each segment, because each segment in p can be any permutation of the elements in the corresponding segment of q (since sorting them will produce the segment in q).
  // So first, we need to check if q can be split into k increasing segments. If not, the answer is 0.
  // Then, compute the product of the factorials of the segment lengths.

  // Find the split points in q where the sequence stops being increasing.
  SetLength(segments, 0);
  m := 0;
  len := 1;
  for i := 1 to n-1 do
  begin
    if qq[i] > qq[i-1] then
      Inc(len)
    else
    begin
      SetLength(segments, Length(segments) + 1);
      segments[High(segments)] := len;
      len := 1;
      Inc(m);
    end;
  end;
  // Add the last segment
  SetLength(segments, Length(segments) + 1);
  segments[High(segments)] := len;
  Inc(m);

  if (m + 1) <> k then
  begin
    Result := 0;
    Exit;
  end;

  // Now, compute the product of the factorials of the segment lengths
  Result := 1;
  for i := 0 to High(segments) do
  begin
    Result := (Result * fact[segments[i]]) mod MOD;
  end;
end;
procedure testCountPermutations;
var
  q1, q2, q3, q4, q5, q6, q7: array of int64;
begin
  q1 := [1, 2];
  Assert(countPermutations(2, 1, q1) = 2);

  q2 := [3, 1, 2];
  Assert(countPermutations(3, 3, q2) = 1);

  q3 := [1, 2, 3, 6, 5, 4];
  Assert(countPermutations(6, 3, q3) = 13);

  q4 := [1, 2, 3, 4, 5, 6];
  Assert(countPermutations(6, 1, q4) = 720);

  q5 := [1, 2, 5, 3, 4, 5];
  Assert(countPermutations(6, 3, q5) = 0);

  q6 := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  Assert(countPermutations(9, 9, q6) = 1);

  q7 := [1, 2, 3, 4, 5, 6, 7, 9, 8];
  Assert(countPermutations(9, 2, q7) = 29093);
end;

begin
  testCountPermutations;
end.