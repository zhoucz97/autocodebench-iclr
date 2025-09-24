{$ASSERTIONS ON}
(*
  Given a non-negative integer num, repeatedly add all its digits until the
  result has only one digit.
  For example:
      >>> addDigits(38)
      2
  Because 3 + 8 = 11, and 1 + 1 = 2. Since 2 has only one digit, 2 is the result.
*)

function addDigits(num: Integer): Integer;
var
  sum: Integer;
begin
  while num >= 10 do
  begin
    sum := 0;
    while num > 0 do
    begin
      sum := sum + (num mod 10);
      num := num div 10;
    end;
    num := sum;
  end;
  addDigits := num;
end;
procedure testAddDigits;
begin
    Assert(addDigits(38) = 2);
    Assert(addDigits(0) = 0);
    Assert(addDigits(9) = 9);
    Assert(addDigits(123) = 6);
    Assert(addDigits(456) = 6);
    Assert(addDigits(9999) = 9);
    Assert(addDigits(100) = 1);
    Assert(addDigits(1010) = 2);
    Assert(addDigits(1234) = 1);
    Assert(addDigits(9876) = 3);
    Assert(addDigits(199) = 1);
end;

begin
    testAddDigits;
end.