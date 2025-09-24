{$ASSERTIONS ON}

uses
  SysUtils, Math;

const
  N = 10000; // Adding a constant for array size
(*
  Given a sequence of student IDs entering a classroom, where the IDs range from 0 to N-1,
  calculate the total number of handshakes that occur. Each student will shake hands with every
  student already in the classroom with a smaller ID number. The sequence represents the order
  in which students enter the classroom.

  Examples:
      >>> countHandshakes(3, [2, 1, 0])
      0
*)

function countHandshakes(n: Integer; order: array of Integer): Int64;
var
  i, j, currentId: Integer;
  presentStudents: array of Boolean;
  count: Int64;
begin
  SetLength(presentStudents, n);
  count := 0;
  
  for i := 0 to High(order) do
  begin
    currentId := order[i];
    // Count how many students in presentStudents have IDs < currentId
    for j := 0 to currentId - 1 do
    begin
      if presentStudents[j] then
        Inc(count);
    end;
    // Mark current student as present
    presentStudents[currentId] := True;
  end;
  
  Result := count;
end;
procedure testCountHandshakes;
begin
  Assert(countHandshakes(4, [2, 1, 3, 0]) = 2);
  Assert(countHandshakes(6, [0, 1, 2, 3, 4, 5]) = 15);
  Assert(countHandshakes(3, [1, 2, 0]) = 1);
  Assert(countHandshakes(4, [3, 2, 1, 0]) = 0);
  Assert(countHandshakes(4, [0, 1, 2, 3]) = 6);
  Assert(countHandshakes(6, [5, 4, 3, 2, 1, 0]) = 0);
  Assert(countHandshakes(4, [0, 2, 1, 3]) = 5);
  Assert(countHandshakes(5, [3, 1, 4, 2, 0]) = 3);
  Assert(countHandshakes(4, [1, 0, 3, 2]) = 4);
  Assert(countHandshakes(3, [2, 0, 1]) = 1);
  Assert(countHandshakes(5, [1, 3, 0, 2, 4]) = 7);
  Assert(countHandshakes(5, [4, 3, 2, 1, 0]) = 0);
end;

begin
  testCountHandshakes;
end.