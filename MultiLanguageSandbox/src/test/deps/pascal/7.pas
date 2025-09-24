{$ASSERTIONS ON}

uses
  SysUtils, Math;
(*
  Calculate the area of a triangle given its base and height.
  Parameters:
  - base (Integer): The base length of the triangle.
  - height (Integer): The height of the triangle.
  Returns:
  Float: The calculated area of the triangle, rounded to one decimal place.
  Example Usage:
  >>> calculate_triangle_area(1, 2)
  1.0
*)

function calculate_triangle_area(base, height: Integer): Double;
begin
  // Calculate the area of the triangle: (base * height) / 2
  calculate_triangle_area := (base * height) / 2;
  
  // Round the result to one decimal place
  calculate_triangle_area := Round(calculate_triangle_area * 10) / 10;
end;
procedure testCalculateTriangleArea;
var
  epsilon: Double;
begin
  epsilon := 1e-6;
  Assert(abs(calculate_triangle_area(1, 2) - 1.0) < epsilon);
  Assert(abs(calculate_triangle_area(3, 4) - 6.0) < epsilon);
  Assert(abs(calculate_triangle_area(5, 8) - 20.0) < epsilon);
  Assert(abs(calculate_triangle_area(7, 3) - 10.5) < epsilon);
  Assert(abs(calculate_triangle_area(10, 10) - 50.0) < epsilon);
end;

begin
  testCalculateTriangleArea;
end.