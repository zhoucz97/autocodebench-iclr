{$ASSERTIONS ON}
(**
 * Calculate the sum of even numbers in a given list.
 * Parameters:
 * - numbers: An array of integers.
 * - size: The number of elements in the array.
 * Returns:
 * The sum of even numbers in the input array.
 *
 * Example usage:
 * >>> calculate_even_sum([1, 4, 3, 2, 5], 5)
 * 6
 *)

function calculate_even_sum(numbers: array of Integer; size: Integer): Integer;
var
  i, sum: Integer;
begin
  sum := 0;
  for i := 0 to size - 1 do
  begin
    if numbers[i] mod 2 = 0 then
      sum := sum + numbers[i];
  end;
  calculate_even_sum := sum;
end;
procedure testCalculateEvenSum;
var
  sample1: array[0..4] of Integer = (1, 4, 3, 2, 5);
  sample2: array[0..3] of Integer = (2, 2, 0, 0);
  sample3: array[0..2] of Integer = (7, 11, 19);
  sample4: array[0..4] of Integer = (12, 14, 16, 18, 20);
begin
  Assert(calculate_even_sum(sample1, 5) = 6);
  Assert(calculate_even_sum(sample2, 4) = 4);
  Assert(calculate_even_sum(sample3, 3) = 0);
  Assert(calculate_even_sum(sample4, 5) = 12 + 14 + 16 + 18 + 20);
end;

begin
  testCalculateEvenSum;
end.