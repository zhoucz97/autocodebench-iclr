{$ASSERTIONS ON}

uses
  SysUtils;
(*
Calculate the Hamming distance between two integers in binary representation.

Parameters:
- x (Integer): The first positive integer (x <= 1,000,000,000).
- y (Integer): The second positive integer (y <= 1,000,000,000).

Returns:
Integer: The Hamming distance between x and y, i.e., the number of bits that 
         need to be flipped to transform x into y in binary.
    >>> hamming_distance(1,2)
    2
*)

function hamming_distance(x, y: Integer): Integer;
var
  xor_result, count: Integer;
begin
  // Calculate the XOR of x and y
  xor_result := x xor y;
  
  // Count the number of set bits (1s) in the XOR result
  count := 0;
  while xor_result > 0 do
  begin
    if (xor_result and 1) = 1 then
      Inc(count);
    xor_result := xor_result shr 1;
  end;
  
  hamming_distance := count;
end;
procedure testHammingDistance;
begin
  Assert(hamming_distance(1, 2) = 2, 'Test Failed: (1, 2)'); // 01 and 10 have 2 different bits
  Assert(hamming_distance(4, 7) = 2, 'Test Failed: (4, 7)'); // 100 and 111 have 2 different bits
  Assert(hamming_distance(25, 30) = 3, 'Test Failed: (25, 30)'); // Additional test: 11001 and 11110 have 3 different bits
  Assert(hamming_distance(0, 0) = 0, 'Test Failed: (0, 0)'); // Additional test: Same numbers have 0 different bits
  Assert(hamming_distance($7FFF, 0) = 15, 'Test Failed: ($7FFF, 0)'); // Additional test: Max unsigned int and 0 have 32 different bits
end;

begin
  testHammingDistance;
end.