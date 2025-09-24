{$ASSERTIONS ON}
(**
  Counts the number of different coloring methods for a series of n squares
  using m colors. The coloring must ensure that adjacent squares, as well
  as the first and last squares, are of different colors.
  
  Parameters:
  - n (integer): The number of squares.
  - m (integer): The number of colors.

  Returns:
  The count of different compatible coloring methods.
  The result is computed modulo 1000003.

  Examples:
    >>> count_coloring_methods(1,1)
    1
*)

function count_coloring_methods(n, m: integer): int64;
const
  MODULO = 1000003;
var
  result: int64;
begin
  if n = 1 then
  begin
    if m >= 1 then
      result := 1
    else
      result := 0;
  end
  else
  begin
    // The formula is (m-1)^n + (-1)^n * (m-1), modulo MODULO
    result := 1;
    // Compute (m-1)^n mod MODULO
    var base := m - 1;
    var power := n;
    while power > 0 do
    begin
      if (power and 1) = 1 then
        result := (result * base) mod MODULO;
      base := (base * base) mod MODULO;
      power := power shr 1;
    end;
    
    // Add (-1)^n * (m-1) mod MODULO
    var term := (m - 1) mod MODULO;
    if (n and 1) = 1 then
      term := (-term) mod MODULO;
    result := (result + term) mod MODULO;
    // Ensure the result is positive
    if result < 0 then
      result := result + MODULO;
  end;
  count_coloring_methods := result;
end;
procedure testCountColoringMethods;
begin
  // WriteLn(count_coloring_methods(1000, 10));
  Assert(count_coloring_methods(1, 1) = 1);
  Assert(count_coloring_methods(2, 2) = 2);
  Assert(count_coloring_methods(3, 3) = 6);
  Assert(count_coloring_methods(4, 2) = 2);
  Assert(count_coloring_methods(1000, 10) = 566585); // Expected result not confirmed
  
  // Additional test cases
  Assert(count_coloring_methods(2, 3) = 6);
  Assert(count_coloring_methods(1000, 1000) = 67911);
  Assert(count_coloring_methods(999, 66) = 501817);
  Assert(count_coloring_methods(5, 3) = 30); // Example of an expected output
end;

begin
  testCountColoringMethods;
  // Uncomment the line below if you want to print a success message
  // Writeln('All tests passed!');
end.