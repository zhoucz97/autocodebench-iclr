{$ASSERTIONS ON}

uses
  SysUtils, Math;
(* 
  Count the number of different permutation schemes for a binary string of length n,
  where the number of '1's is m and the number of '0's is n - m.

  This function accepts two parameters:
  - n (integer): The length of the binary string.
  - m (integer): The number of '1's in the binary string.

  It returns an Int64 indicating the total number of different permutation schemes.
*)

function countPermutationsOfBinaryString(n, m: integer): Int64;
var
  i: integer;
  result: Int64;
begin
  if (m < 0) or (m > n) then
  begin
    countPermutationsOfBinaryString := 0;
    Exit;
  end;
  
  // To optimize, we choose the smaller of m and n-m to minimize the number of multiplications
  if m > n - m then
    m := n - m;
  
  result := 1;
  for i := 1 to m do
  begin
    result := result * (n - m + i) div i;
  end;
  
  countPermutationsOfBinaryString := result;
end;
procedure testCountPermutationsOfBinaryString;
begin
  Assert(countPermutationsOfBinaryString(2, 0) = 2);
  Assert(countPermutationsOfBinaryString(2, 1) = 0);
  Assert(countPermutationsOfBinaryString(3, 0) = 0);
  Assert(countPermutationsOfBinaryString(3, 1) = 3);
  Assert(countPermutationsOfBinaryString(4, 2) = 4);
  Assert(countPermutationsOfBinaryString(5, 5) = 1);
  Assert(countPermutationsOfBinaryString(30, 2) = 145422675);
  Assert(countPermutationsOfBinaryString(33, 17) = 13884156);
  Assert(countPermutationsOfBinaryString(1000, 1000) = 1);
  // Add more test cases if necessary
end;

(* Main *)
begin
  testCountPermutationsOfBinaryString;
  // WriteLn('All test cases passed.');
  // ReadLn;
end.