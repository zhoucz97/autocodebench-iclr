{$ASSERTIONS ON}

uses
  SysUtils;
(*
Given n positive integers representing the count of each number from 1 to n, 
find the maximum sum of the mode for all prefixes of a sequence constructed from these numbers. 
The mode is the largest number among the most frequent elements in a sequence.
Example:
    >>> maxModeSum(3, [1, 2, 3])
    Should return 17
    An example sequence that reaches this maximum value is (3,2,3,1,2,2).
*)

function maxModeSum(n: int64; const counts: array of int64): int64;
var
  i, j, k, currentMaxFreq, currentMode, sum: int64;
  freq: array of int64;
begin
  sum := 0;
  SetLength(freq, n + 1); // freq[1..n]
  
  // Initialize frequency array
  for i := 1 to n do
    freq[i] := 0;
  
  currentMaxFreq := 0;
  currentMode := 0;
  
  // We process numbers from highest to lowest to maximize the mode in prefixes
  for i := n downto 1 do
  begin
    for j := 1 to counts[i - 1] do
    begin
      freq[i] := freq[i] + 1;
      if (freq[i] > currentMaxFreq) or 
         (freq[i] = currentMaxFreq) and (i > currentMode) then
      begin
        currentMaxFreq := freq[i];
        currentMode := i;
      end;
      sum := sum + currentMode;
    end;
  end;
  
  maxModeSum := sum;
end;
procedure testMaxModeSum;
begin
  Assert(maxModeSum(3, [1, 3, 2]) = 17);
  Assert(maxModeSum(4, [4, 1, 2, 3]) = 37);
  Assert(maxModeSum(2, [1, 1]) = 4);
  Assert(maxModeSum(5, [1, 2, 3, 4, 5]) = 75);
  Assert(maxModeSum(1, [100000]) = 100000);
  Assert(maxModeSum(5, [5, 3, 2, 4, 1]) = 62);
  Assert(maxModeSum(3, [100000, 100000, 100000]) = 900000);
  Assert(maxModeSum(3, [2, 2, 5]) = 27);
  Assert(maxModeSum(4, [4, 4, 4, 4]) = 64);
  Assert(maxModeSum(6, [1, 2, 3, 4, 5, 6]) = 126);
  Assert(maxModeSum(3, [3, 1, 2]) = 16);
end;

(* Main *)
begin
  testMaxModeSum;
end.