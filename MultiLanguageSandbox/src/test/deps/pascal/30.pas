{$ASSERTIONS ON}

uses
  SysUtils;
(*
  Checks if a three-character string has exactly two characters that are the same.

  Args:
    s: The three-character string to be checked.
  
  Returns:
    Returns 'Yes' if the input has exactly two equal characters, otherwise 'No'.
    >>> check_two_equal_digits('112')
    Yes
*)

function check_two_equal_digits(const s: string): string;
begin
  // Check if the first and second characters are the same and the third is different
  if (s[1] = s[2]) and (s[1] <> s[3]) then
    Result := 'Yes'
  // Check if the first and third characters are the same and the second is different
  else if (s[1] = s[3]) and (s[1] <> s[2]) then
    Result := 'Yes'
  // Check if the second and third characters are the same and the first is different
  else if (s[2] = s[3]) and (s[2] <> s[1]) then
    Result := 'Yes'
  else
    Result := 'No';
end;
procedure testCheckTwoEqualDigits;
begin
  Assert(check_two_equal_digits('112') = 'Yes');
  Assert(check_two_equal_digits('123') = 'No');
  Assert(check_two_equal_digits('232') = 'Yes');
  Assert(check_two_equal_digits('444') = 'No'); // All three characters are equal, not two.
  Assert(check_two_equal_digits('121') = 'Yes');

  // Additional test cases to cover more scenarios
  Assert(check_two_equal_digits('787') = 'Yes');
  Assert(check_two_equal_digits('999') = 'No'); // All three characters are equal
  Assert(check_two_equal_digits('890') = 'No');
  Assert(check_two_equal_digits('556') = 'Yes');
  Assert(check_two_equal_digits('353') = 'Yes'); // No two characters are equal
end;

begin
  testCheckTwoEqualDigits;
end.