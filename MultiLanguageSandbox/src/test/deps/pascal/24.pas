{$ASSERTIONS ON}

uses
  SysUtils;
(*
  Constructs a formatted string containing the given integer and character separated by a comma.
  
  Parameters:
    - integer_value : Integer; The input integer.
    - char_value    : Char; The input character.
  
  Returns:
    - String: The formatted string representing the integer and character separated by a comma.
  
  Example Usage:
    > process_integer_and_char(234, 'H')
    '234,H'
*)

function process_integer_and_char(integer_value: Integer; char_value: Char): String;
begin
  // Convert the integer to a string and concatenate it with the character, separated by a comma
  process_integer_and_char := IntToStr(integer_value) + ',' + char_value;
end;
procedure testProcessIntegerAndChar;
var
  testOutput: String;
begin
  // Test with expected output "234,H"
  testOutput := process_integer_and_char(234, 'H');
  Assert(testOutput = '234,H', 'Test 1 failed');
  
  // Additional test cases can be provided below
  
  // Test with expected output "123,A"
  testOutput := process_integer_and_char(123, 'A');
  Assert(testOutput = '123,A', 'Test 2 failed');

  // Test with expected output "0,Z"
  testOutput := process_integer_and_char(0, 'Z');
  Assert(testOutput = '0,Z', 'Test 3 failed');
end;

(* Main Program *)
begin
  testProcessIntegerAndChar;
end.