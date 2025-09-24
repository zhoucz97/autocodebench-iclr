{$ASSERTIONS ON}

uses
  Math, SysUtils;
(*
  Determines the number of acute-angled triangles that can be formed from a given set of edge lengths.
  
  Parameters:
  - edges: An open array of integers representing edge lengths.

  Returns:
  The count of distinct, valid acute-angled triangles that can be formed.
  
  Examples:
  writeln(count_acute_triangles([1, 1, 1, 1])); (* Output: 4 *)
*)

function count_acute_triangles(var edges: array of integer): integer;
var
  i, j, k, n, count: integer;
  a, b, c: integer;
begin
  n := Length(edges);
  count := 0;
  
  // Sort the array to facilitate checking conditions
  for i := 0 to n - 2 do
    for j := i + 1 to n - 1 do
      if edges[i] > edges[j] then
      begin
        a := edges[i];
        edges[i] := edges[j];
        edges[j] := a;
      end;
  
  // Iterate through all possible triplets
  for i := 0 to n - 3 do
    for j := i + 1 to n - 2 do
      for k := j + 1 to n - 1 do
      begin
        a := edges[i];
        b := edges[j];
        c := edges[k];
        
        // Check triangle inequality: a + b > c
        if a + b > c then
        begin
          // Check for acute triangle: a^2 + b^2 > c^2
          if (a * a + b * b) > (c * c) then
            Inc(count);
        end;
      end;
  
  Result := count;
end;
procedure testCountAcuteTriangles;
var
  test_array: array of integer;
begin
  SetLength(test_array, 4);test_array[0] := 1;test_array[1] := 1;test_array[2] := 1;test_array[3] := 1;
  Assert(count_acute_triangles(test_array) = 4);
  SetLength(test_array, 3);test_array[0] := 1;test_array[1] := 2;test_array[2] := 3; 
  Assert(count_acute_triangles(test_array) = 0);
  SetLength(test_array, 5);test_array[0] := 3;test_array[1] := 4;test_array[2] := 5;test_array[3] := 7;test_array[4] := 10;
  Assert(count_acute_triangles(test_array) = 0);
  SetLength(test_array, 6);test_array[0] := 6;test_array[1] := 8;test_array[2] := 10;test_array[3] := 5;test_array[4] := 5;test_array[5] := 5;
  Assert(count_acute_triangles(test_array) = 4);
end;

begin
  testCountAcuteTriangles;
end.