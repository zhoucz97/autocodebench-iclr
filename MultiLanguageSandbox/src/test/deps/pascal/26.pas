{$ASSERTIONS ON}

uses
  SysUtils, StrUtils;
(*
  Classify the integer x as follows:
    - If x is a single-digit number, x belongs to its own class.
    - Otherwise, sum the digits of x, get a new x, and continue this process iteratively 
      until the class is determined.

  @param x: The string representation of the integer to be classified.
  @return: The class to which the integer x belongs.

  Examples:
      >>> classify_integer('24')
      6
*)

var

function classify_integer(x: string): Integer;
var
  num: Integer;
  digit: Integer;
  sum: Integer;
begin
  // Convert the string to an integer
  num := StrToInt(x);
  
  // Handle the case where the number is 0
  if num = 0 then
  begin
    classify_integer := 0;
    Exit;
  end;
  
  // Process the number until it becomes a single digit
  while num >= 10 do
  begin
    sum := 0;
    // Sum all digits of the current number
    while num > 0 do
    begin
      digit := num mod 10;
      sum := sum + digit;
      num := num div 10;
    end;
    num := sum;
  end;
  
  classify_integer := num;
end;
procedure testClassifyInteger;
begin
  Assert(classify_integer('24') = 6);
  Assert(classify_integer('39') = 3);
  Assert(classify_integer('123456789') = 9);
  Assert(classify_integer('123456789012345678901234567890') = 9);
  Assert(classify_integer('12345') = 6);
  Assert(classify_integer('999999999') = 9);
end;

begin
  testClassifyInteger;
end.