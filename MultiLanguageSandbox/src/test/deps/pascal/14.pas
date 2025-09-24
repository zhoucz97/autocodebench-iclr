{$ASSERTIONS ON}
(*
  Decode a series of numbers to reveal the pattern and understand the actual values
  each digit represents.
  0000=4 8888=8 1234=1 5678=3 9012=2 1357=0 2468=4
  
  Parameters:
  - data_str: A string representing a series of numbers. Length does not exceed 100.
  
  Returns:
  integer: The result of each data string corresponding to the right-hand side of the equation.
  
  Example usage:
  Assert(decodeNumbers('0000') = 4);
*)

function decodeNumbers(data_str: AnsiString): Integer;
var
  i: Integer;
  digit: Char;
  loopCount: Integer;
begin
  loopCount := 0;
  for i := 1 to Length(data_str) do
  begin
    digit := data_str[i];
    case digit of
      '0': loopCount := loopCount + 1;
      '1': loopCount := loopCount + 0;
      '2': loopCount := loopCount + 0;
      '3': loopCount := loopCount + 0;
      '4': loopCount := loopCount + 1;
      '5': loopCount := loopCount + 0;
      '6': loopCount := loopCount + 1;
      '7': loopCount := loopCount + 0;
      '8': loopCount := loopCount + 2;
      '9': loopCount := loopCount + 1;
    end;
  end;
  decodeNumbers := loopCount;
end;
procedure testDecodeNumbers;
begin
  Assert(decodeNumbers('0000') = 4);
  Assert(decodeNumbers('8888') = 8);
  Assert(decodeNumbers('1234') = 1);
  Assert(decodeNumbers('5678') = 3);
  Assert(decodeNumbers('9012') = 2);
  Assert(decodeNumbers('1357') = 0);
  Assert(decodeNumbers('2468') = 4);
  // Additional test samples
  Assert(decodeNumbers('9999') = 4);
  Assert(decodeNumbers('1111') = 0);
  Assert(decodeNumbers('2222') = 0);
  Assert(decodeNumbers('3333') = 0);
  Assert(decodeNumbers('4444') = 4);
  Assert(decodeNumbers('5555') = 0);
  Assert(decodeNumbers('6666') = 4);
  Assert(decodeNumbers('7777') = 0);
  Assert(decodeNumbers('0001') = 3);
  Assert(decodeNumbers('2301') = 1);
end;

begin
  testDecodeNumbers;
end.