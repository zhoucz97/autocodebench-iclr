
(*
  Determines the maximum and minimum of three distinct integers.

  Parameters:
  a, b, c (integers): Input values among which max and min are to be found.

  This procedure assigns the maximum to the variable 'max', and the minimum to the variable 'min'. Since Pascal does not support returning multiple values directly, we pass parameters by reference.

  Examples:
  var max, min: integer;
  find_max_min(1, 2, 3, max, min);
  Assert((max = 3) and (min = 1));
*)

procedure find_max_min(a, b, c: integer; var max, min: integer);
begin
    // Initialize max and min with the first value
    max := a;
    min := a;

    // Compare with the second value
    if b > max then
        max := b
    else if b < min then
        min := b;

    // Compare with the third value
    if c > max then
        max := c
    else if c < min then
        min := c;
end;
procedure TestFindMaxMin;
var
  max, min: integer;
begin
  find_max_min(1, 2, 3, max, min);
  Assert((max = 3) and (min = 1), 'Test case 1 failed');

  find_max_min(5, 3, 4, max, min);
  Assert((max = 5) and (min = 3), 'Test case 2 failed');

  find_max_min(10, -2, 7, max, min);
  Assert((max = 10) and (min = -2), 'Test case 3 failed');

  find_max_min(-1, -3, -2, max, min);
  Assert((max = -1) and (min = -3), 'Test case 4 failed');
end;

begin
  TestFindMaxMin;
  WriteLn('All tests passed.');
end.