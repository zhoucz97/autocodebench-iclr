{$MODE OBJFPC}{$H+}{$ASSERTIONS ON}
(*
  Determine if two closed intervals intersect.
    Arguments:
    a, b: Represent the first closed interval [a, b] where 0 <= a <= b <= 1000.
    c, d: Represent the second closed interval [c, d] where 0 <= c <= d <= 1000.
    Returns:
    An integer: 1 if the intervals intersect, 0 otherwise.
*)

function AreIntervalsIntersecting(a, b, c, d: Integer): Integer;
begin
  // Check if the intervals intersect
  if (b >= c) and (d >= a) then
    AreIntervalsIntersecting := 1  // Intervals intersect
  else
    AreIntervalsIntersecting := 0; // Intervals do not intersect
end;
procedure TestAreIntervalsIntersecting;
begin
  Assert(AreIntervalsIntersecting(1, 1, 1, 2) = 1);
  Assert(AreIntervalsIntersecting(3, 5, 2, 6) = 1);
  Assert(AreIntervalsIntersecting(3, 5, 4, 7) = 1);
  Assert(AreIntervalsIntersecting(3, 5, 6, 7) = 0);
  // Additional test cases
  Assert(AreIntervalsIntersecting(0, 0, 0, 0) = 1);
  Assert(AreIntervalsIntersecting(1, 3, 2, 4) = 1);
  Assert(AreIntervalsIntersecting(1, 3, 4, 6) = 0);
  Assert(AreIntervalsIntersecting(10, 20, 20, 30) = 1);
  Assert(AreIntervalsIntersecting(10, 20, 21, 30) = 0);
end;

begin
  TestAreIntervalsIntersecting;
end.