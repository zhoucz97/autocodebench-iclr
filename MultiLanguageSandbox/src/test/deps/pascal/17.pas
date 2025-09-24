uses
  SysUtils;
(*
  Find the length of the longest consecutive sequence of 1s in the binary
  representation of a non-negative integer.
  
    Parameters:
    - n: an integer (0 ≤ n ≤ 2^64 - 1).
    
    Returns:
    The length of the longest consecutive sequence of 1s in binary.
    
    Examples:
    >>> find_longest_consecutive_ones_length(7)
    3
*)

function find_longest_consecutive_ones_length(n: UInt64): integer;
var
  currentLength, maxLength: integer;
begin
  currentLength := 0;
  maxLength := 0;
  
  while n > 0 do
  begin
    if (n and 1) = 1 then
    begin
      currentLength := currentLength + 1;
      if currentLength > maxLength then
        maxLength := currentLength;
    end
    else
      currentLength := 0;
    
    n := n shr 1;
  end;
  
  find_longest_consecutive_ones_length := maxLength;
end;
procedure testFindLongestConsecutiveOnesLength;
begin
  Assert(find_longest_consecutive_ones_length(7) = 3);
  Assert(find_longest_consecutive_ones_length(13) = 2);
  Assert(find_longest_consecutive_ones_length(12345) = 3); // New test sample
  // Assert(find_longest_consecutive_ones_length(bin2dec('11011101111')) = 4); // New test sample: binary literal conversion
  Assert(find_longest_consecutive_ones_length($FFFFFFFF) = 32); // New test sample: all ones for a 32-bit number
  Assert(find_longest_consecutive_ones_length(0) = 0); // New test sample: no ones in a zero
end;

begin
  testFindLongestConsecutiveOnesLength;
  // Writeln('All tests passed!');
end.