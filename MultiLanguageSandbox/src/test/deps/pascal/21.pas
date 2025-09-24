{$ASSERTIONS ON}

uses
  Math, SysUtils;
(*
The task is to determine if it is possible to assemble the wooden squares from n buckets,
where each bucket contains a number of squares with a side length of 1, into a single larger square.
The function receives the length of the list and an array of numbers as input.
Example usage:
    Is_Square(1, [9]) should return true (1 in Pascal), because 9 is a perfect square.
*)

function Is_Square(length: integer; const squares: array of integer): boolean;
var
  total, i, k: integer;
begin
  total := 0;
  for i := 0 to length - 1 do
    total := total + squares[i];
  
  k := Trunc(Sqrt(total));
  if k * k = total then
    Is_Square := True
  else
    Is_Square := False;
end;
procedure TestIsSquare;
begin
  Assert(Is_Square(1, [9]) = true);
  Assert(Is_Square(2, [14, 2]) = true);
  Assert(Is_Square(2, [7, 7]) = false);
  Assert(Is_Square(7, [1, 2, 3, 4, 5, 6, 7]) = false);
  Assert(Is_Square(6, [1, 3, 5, 7, 9, 11]) = true);
  Assert(Is_Square(4, [2, 2, 2, 2]) = false);
  Assert(Is_Square(3, [4, 5, 6]) = false);
  Assert(Is_Square(4, [16, 9, 4, 1]) = false);
  Assert(Is_Square(5, [1, 1, 1, 1, 1]) = false);
  Assert(Is_Square(2, [25, 25]) = false);
  Assert(Is_Square(3, [10, 10, 5]) = true);
end;

begin
  TestIsSquare;
end.