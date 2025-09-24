{$ASSERTIONS ON}
{
  Transforms the case of a given letter.
  Parameters:
  - letter (Char): The input letter to be transformed.
  Returns:
  - Char: The letter with its case reversed. If the input is lowercase, it returns the uppercase version,
  and if the input is uppercase, it returns the lowercase version.
    Example usage:
    >>> transform_letter_case('b')
    'B'
}

function transform_letter_case(letter: Char): Char;
begin
  if (letter >= 'a') and (letter <= 'z') then
    transform_letter_case := UpCase(letter)
  else if (letter >= 'A') and (letter <= 'Z') then
    transform_letter_case := DownCase(letter)
  else
    transform_letter_case := letter; // Return the same character if it's not a letter
end;
procedure testTransformLetterCase;
begin
  Assert(transform_letter_case('b') = 'B');
  Assert(transform_letter_case('B') = 'b');
  
  // Additional test cases
  Assert(transform_letter_case('z') = 'Z');
  Assert(transform_letter_case('Z') = 'z');
  Assert(transform_letter_case('m') = 'M');
  Assert(transform_letter_case('M') = 'm');
  Assert(transform_letter_case('1') = '1'); // Non-alphabetic, should be unchanged
  Assert(transform_letter_case('!') = '!'); // Non-alphabetic, should be unchanged
end;

begin
  testTransformLetterCase;
end.