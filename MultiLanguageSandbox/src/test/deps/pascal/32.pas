{$ASSERTIONS ON}
(*
Given a sequence of n events, each occurring with a periodicity of a_i years,
the problem is to find the year Yn when the nth event occurs, under the
condition that the countdown for event i+1 can only commence in the year
immediately following the occurrence of event i.
    >>> apocalypseYear(6, [3, 2, 4, 5, 9, 18])
    36
*)

function apocalypseYear(n: Int64; signs: array of Int64): Int64;
var
  i: Int64;
  currentYear: Int64;
begin
  if n = 0 then
  begin
    apocalypseYear := 0;
    Exit;
  end;
  
  currentYear := 0;
  for i := 0 to n - 2 do
  begin
    // The next event must occur in a year >= currentYear + 1 and divisible by signs[i]
    // So the minimal such year is the smallest multiple of signs[i] >= currentYear + 1
    currentYear := ((currentYear + 1) div signs[i]) * signs[i];
    if (currentYear + 1) mod signs[i] <> 0 then
      currentYear := currentYear + (signs[i] - (currentYear + 1) mod signs[i]);
  end;
  
  apocalypseYear := currentYear;
end;
procedure testApocalypseYear;
begin
  Assert(apocalypseYear(6, [3, 2, 4, 5, 9, 18]) = 36);
  Assert(apocalypseYear(5, [1, 2, 3, 4, 5]) = 5);
  Assert(apocalypseYear(5, [1, 1, 1, 1, 1]) = 5);
  Assert(apocalypseYear(6, [50, 30, 711, 200, 503, 1006]) = 2012);
  Assert(apocalypseYear(2, [1, 2]) = 2);
  Assert(apocalypseYear(3, [3, 1, 2]) = 6);
  Assert(apocalypseYear(3, [2, 3, 4]) = 4);
  Assert(apocalypseYear(4, [1, 2, 3, 4]) = 4);
  Assert(apocalypseYear(4, [5, 7, 11, 13]) = 13);
  Assert(apocalypseYear(5, [2, 2, 2, 2, 2]) = 10);
  Assert(apocalypseYear(3, [6, 10, 15]) = 15);
  Assert(apocalypseYear(3, [4, 6, 14]) = 14);
  Assert(apocalypseYear(4, [50, 30, 711, 200]) = 800);
  Assert(apocalypseYear(6, [1, 1, 1, 1, 1, 1]) = 6);
  Assert(apocalypseYear(2, [1000000, 999999]) = 1999998);
end;

begin
  testApocalypseYear;
end.