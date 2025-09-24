{$ASSERTIONS ON}

(* Pascal does not have a direct equivalent to <math.h> or <string.h>, but its built-in math support will suffice for this conversion. SysUtils can be used for extended system functionality. *)

uses
  SysUtils, Math;
(**
 * Calculate the distance between two points A (xa, ya) and B (xb, yb).
 *
 * Parameters:
 * - xa (Double): x-coordinate of point A.
 * - ya (Double): y-coordinate of point A.
 * - xb (Double): x-coordinate of point B.
 * - yb (Double): y-coordinate of point B.
 *
 * Returns:
 * Double: The distance between points A and B.
 *
 * Examples:
 * >>> calculate_distance(0,0,3,4)
 * 5
 *)

function calculate_distance(xa, ya, xb, yb: Double): Double;
begin
  // Calculate the differences in x and y coordinates
  var dx := xb - xa;
  var dy := yb - ya;
  
  // Calculate the square of the differences
  var dx_squared := dx * dx;
  var dy_squared := dy * dy;
  
  // Sum the squares and take the square root to get the distance
  calculate_distance := Sqrt(dx_squared + dy_squared);
end;
procedure testCalculateDistance;
var
  epsilon: Double;
begin
  epsilon := 1e-6;
  Assert(abs(calculate_distance(0, 0, 3, 4) - 5) < epsilon);
  Assert(abs(calculate_distance(0, 0, 0, 0) - 0) < epsilon);
  Assert(abs(calculate_distance(-1, -1, 2, 2) - 4.242640687) < epsilon);
  Assert(abs(calculate_distance(1.5, 3.9, 4.2, -1.1) - 5.68243) < epsilon);
  // writeln('All test cases passed!');
end;

begin
  testCalculateDistance;
end.