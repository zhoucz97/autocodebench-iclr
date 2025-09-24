{$ASSERTIONS ON}

uses
  SysUtils, Math;
(*
Given three integers a, b, and c, with two of them being equal and the third one being distinct,
the task is to identify and return the distinct integer that occurs exactly once.
Examples:
>>> extraNumber(0,0,1) returns 1
>>> extraNumber(4,3,4) returns 3
*)

function extraNumber(a, b, c: integer): integer;
begin
    if a = b then
        extraNumber := c
    else if a = c then
        extraNumber := b
    else
        extraNumber := a;
end;
procedure testExtraNumber;
begin
    Assert(extraNumber(2, 7, 2) = 7);
    Assert(extraNumber(3, 2, 2) = 3);
    Assert(extraNumber(5, 5, 1) = 1);
    Assert(extraNumber(500000000, 3, 500000000) = 3);
    Assert(extraNumber(500000000, 500000000, 3) = 3);
end;

begin
    testExtraNumber;
    WriteLn('All test cases passed!');
end.