{$ASSERTIONS ON}

uses 
  Math, SysUtils;
(*
Calculates the area of a triangle given the lengths of its three sides.

Parameters:
- a (real): Length of side 'a'.
- b (real): Length of side 'b'.
- c (real): Length of side 'c'.

Returns:
- real: If the sides can form a triangle, returns the calculated area rounded to 2 decimal places.
         Otherwise, returns -1 indicating that the sides do not form a triangle.

Examples:
    >>> calculate_triangle_area(3, 5, 4)
    6.00
*)

var

function calculate_triangle_area(a, b, c: real): real;
var
  s, area: real;
begin
  // Check if the sides can form a triangle (triangle inequality theorem)
  if (a + b > c) and (a + c > b) and (b + c > a) then
  begin
    // Calculate semi-perimeter
    s := (a + b + c) / 2;
    
    // Calculate area using Heron's formula
    area := sqrt(s * (s - a) * (s - b) * (s - c));
    
    // Round to 2 decimal places
    calculate_triangle_area := round(area * 100) / 100;
  end
  else
  begin
    // Invalid triangle
    calculate_triangle_area := -1;
  end;
end;
procedure testCalculateTriangleArea;
var
  area: real;
begin
  (* Triangle with sides 3, 5, 4 should return area 6 *)
  area := calculate_triangle_area(3, 5, 4);
  Assert(abs(area - 6) < 1e-6);

  (* Not a triangle with sides 1, 1, 4 should return -1 *)
  area := calculate_triangle_area(1, 1, 4);
  Assert(area = -1);

  (* Triangle with sides 7, 24, 25 should return area 84 *)
  area := calculate_triangle_area(7, 24, 25);
  Assert(abs(area - 84) < 1e-6);

  (* Triangle with sides 10.5, 6.2, 7.3 should return a calculated area *)
  area := calculate_triangle_area(10.5, 6.2, 7.3);
  (* For precision issues, we test if the calculated area is close enough to the expected value *)
  Assert(abs(area - 22.15) < 1e-2);
end;

(* Main *)
begin
  testCalculateTriangleArea;
end.