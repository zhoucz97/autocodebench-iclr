{$ASSERTIONS ON}
{
  Counts the number of odd integers in a given list of numbers.
  Parameters:
    - count: The number of integers to evaluate.
    - numbers: An open array of integers.
  Returns:
    The count of odd numbers in the input array.
  Example:
    >>> count_odd_numbers(5, [1, 4, 3, 2, 5])
    Result: 3
}

function count_odd_numbers(count: Integer; const numbers: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to count - 1 do
  begin
    if (numbers[i] mod 2) <> 0 then
      Inc(Result);
  end;
end;
procedure testCountOddNumbers;
begin
  Assert(count_odd_numbers(5, [1, 4, 3, 2, 5]) = 3);
  Assert(count_odd_numbers(4, [2, 2, 0, 0]) = 0);
  Assert(count_odd_numbers(6, [7, 7, 8, 1, 9, 10]) = 4); // Additional Test Sample
end;

begin
  testCountOddNumbers;
  // WriteLn('All tests passed!');
end.