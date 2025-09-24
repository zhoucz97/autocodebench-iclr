{$ASSERTIONS ON}
(*
Given integers c and d, with the conditions a + b = c and a * b = d, determine a
possible value for integer a (where a <= b). If multiple pairs (a, b) satisfy
the conditions, choose the one with the smallest value of a. If there are no such
pairs, return -1.

    Parameters:
    - c (integer): The sum of a and b.
    - d (integer): The product of a and b.

    Returns:
    An integer representing a possible value of a, or -1 if no valid pairs are found.

    Examples:
    - find_integers(7, 11): -1
*)

function find_integers(c, d: integer): integer;
var
  discriminant, sqrt_discriminant, a, b: integer;
begin
  discriminant := c * c - 4 * d;
  
  if discriminant < 0 then
  begin
    find_integers := -1;
    Exit;
  end;
  
  sqrt_discriminant := Trunc(Sqrt(discriminant));
  
  if sqrt_discriminant * sqrt_discriminant <> discriminant then
  begin
    find_integers := -1;
    Exit;
  end;
  
  a := (c - sqrt_discriminant) div 2;
  b := (c + sqrt_discriminant) div 2;
  
  // Verify that a and b are integers and satisfy the original equations
  if (a + b = c) and (a * b = d) then
    find_integers := a
  else
    find_integers := -1;
end;
procedure testFindIntegers;
begin
    Assert(find_integers(5, 6) = 2);
    Assert(find_integers(6, 9) = 3);
    Assert(find_integers(7, 12) = 3);
    Assert(find_integers(7, 11) = -1);
    Assert(find_integers(9, 8) = 1);
    Assert(find_integers(10, 25) = 5);
    Assert(find_integers(10000, 8765) = -1);
end;

begin
    testFindIntegers;
end.